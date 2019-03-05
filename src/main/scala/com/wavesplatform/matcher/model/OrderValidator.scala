package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.matcher.error._
import com.wavesplatform.matcher.smart.MatcherScriptRunner
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.utils.Time
import kamon.Kamon
import shapeless.Coproduct

import scala.util.control.NonFatal

object OrderValidator {

  type Result[T] = Either[MatcherError, T]

  private val timer = Kamon.timer("matcher.validation").refine("type" -> "blockchain")

  val MinExpiration: Long   = 60 * 1000L
  val MaxActiveOrders: Long = 200

  private def verifySignature(order: Order): Result[Order] =
    Verifier.verifyAsEllipticCurveSignature(order).leftMap(x => MatcherError.OrderInvalidSignature(order.id(), x.toString))

  private def verifyOrderByAccountScript(blockchain: Blockchain, address: Address, order: Order): Result[Order] =
    blockchain.accountScript(address).fold(verifySignature(order)) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
        MatcherError.ScriptedAccountTradingUnsupported.asLeft
      else if (order.version <= 1) MatcherError.OrderVersionUnsupportedWithScriptedAccount(address).asLeft
      else
        try MatcherScriptRunner(script, order, isTokenScript = false) match {
          case (_, Left(execError)) => MatcherError.AccountScriptReturnedError(address, execError).asLeft
          case (_, Right(FALSE))    => MatcherError.AccountScriptDeniedOrder(address).asLeft
          case (_, Right(TRUE))     => order.asRight
          case (_, Right(x))        => MatcherError.AccountScriptUnexpectResult(address, x.toString).asLeft
        } catch {
          case NonFatal(e) => MatcherError.AccountScriptException(address, e.getClass.getCanonicalName, e.getMessage).asLeft
        }
    }

  private def verifySmartToken(blockchain: Blockchain, assetId: AssetId, tx: ExchangeTransaction): Result[Unit] =
    blockchain.assetScript(assetId).fold(success) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets, blockchain.height))
        MatcherError.ScriptedAssetTradingUnsupported(assetId).asLeft
      else
        try ScriptRunner(blockchain.height, Coproduct(tx), blockchain, script, isTokenScript = true) match {
          case (_, Left(execError)) => MatcherError.AssetScriptReturnedError(assetId, execError).asLeft
          case (_, Right(FALSE))    => MatcherError.AssetScriptDeniedOrder(assetId).asLeft
          case (_, Right(TRUE))     => ().asRight
          case (_, Right(x))        => MatcherError.AssetScriptUnexpectResult(assetId, x.toString).asLeft
        } catch {
          case NonFatal(e) => MatcherError.AssetScriptException(assetId, e.getClass.getCanonicalName, e.getMessage).asLeft
        }
    }

  private def decimals(blockchain: Blockchain, assetId: Option[AssetId]): Result[Int] =
    assetId.fold(lift(8)) { aid =>
      blockchain.assetDescription(aid).map(_.decimals).toRight(MatcherError.AssetNotFound(aid))
    }

  private def validateDecimals(blockchain: Blockchain, o: Order): Result[Unit] =
    for {
      pd <- decimals(blockchain, o.assetPair.priceAsset)
      ad <- decimals(blockchain, o.assetPair.amountAsset)
      insignificantDecimals = (pd - ad).max(0)
      _ <- Either.cond(
        o.price % BigDecimal(10).pow(insignificantDecimals).toLongExact == 0,
        (),
        MatcherError.PriceLastDecimalsMustBeZero(insignificantDecimals)
      )
    } yield ()

  def blockchainAware(
      blockchain: Blockchain,
      transactionCreator: (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction],
      orderMatchTxFee: Long,
      matcherAddress: Address,
      time: Time,
  )(order: Order): Result[Order] = timer.measure {
    lazy val exchangeTx: Result[ExchangeTransaction] = {
      val fakeOrder: Order = order match {
        case x: OrderV1 => x.copy(orderType = x.orderType.opposite)
        case x: OrderV2 => x.copy(orderType = x.orderType.opposite)
      }
      transactionCreator(LimitOrder(fakeOrder), LimitOrder(order), time.correctedTime()).left.map { x =>
        MatcherError.CanNotCreateExchangeTransaction(x.toString)
      }
    }

    def verifyAssetScript(assetId: Option[AssetId]): Result[Unit] = assetId.fold(success) { assetId =>
      exchangeTx.flatMap(verifySmartToken(blockchain, assetId, _))
    }

    lazy val mof = ExchangeTransactionCreator.minFee(blockchain, orderMatchTxFee, matcherAddress, order.assetPair)
    for {
      _ <- lift(order)
        .ensure(MatcherError.OrderVersionUnsupported(order.version, BlockchainFeatures.SmartAccountTrading)) {
          _.version == 1 || blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height)
        }
        .ensure(MatcherError.FeeNotEnough(mof, order.matcherFee, None))(_.matcherFee >= mof)
      _ <- validateDecimals(blockchain, order)
      _ <- verifyOrderByAccountScript(blockchain, order.sender, order)
      _ <- verifyAssetScript(order.assetPair.amountAsset)
      _ <- verifyAssetScript(order.assetPair.priceAsset)
    } yield order
  }

  private def validateBalance(order: Order, tradableBalance: Option[AssetId] => Long): Result[Order] = {
    val lo               = LimitOrder(order)
    val requiredForOrder = lo.requiredBalance

    val available = requiredForOrder.keySet.map { assetId =>
      assetId -> tradableBalance(assetId)
    }.toMap

    val negativeBalances = Monoid.combine(available, requiredForOrder.mapValues(-_)).filter(_._2 < 0)
    Either.cond(negativeBalances.isEmpty, order, MatcherError.BalanceNotEnough(requiredForOrder, available))
  }

  def matcherSettingsAware(
      matcherPublicKey: PublicKeyAccount,
      blacklistedAddresses: Set[Address],
      blacklistedAssets: Set[AssetId],
  )(order: Order): Result[Order] =
    for {
      _ <- lift(order)
        .ensure(MatcherError.UnexpectedMatcherPublicKey(matcherPublicKey, order.matcherPublicKey))(_.matcherPublicKey == matcherPublicKey)
        .ensure(MatcherError.AddressIsBlacklisted(order.sender))(o => !blacklistedAddresses.contains(o.sender.toAddress))
      _ <- order.assetPair.amountAsset.fold(success)(x => Either.cond(!blacklistedAssets(x), (), MatcherError.AmountAssetIsBlacklisted(x)))
      _ <- order.assetPair.priceAsset.fold(success)(x => Either.cond(!blacklistedAssets(x), (), MatcherError.PriceAssetIsBlacklisted(x)))
    } yield order

  def timeAware(time: Time)(order: Order): Result[Order] = {
    for {
      _ <- Either.cond(order.expiration > time.correctedTime() + MinExpiration,
                       (),
                       MatcherError.WrongExpiration(time.correctedTime(), MinExpiration, order.expiration))
      _ <- order.isValid(time.correctedTime()).toEither.left.map(MatcherError.OrderCommonValidationFailed)
    } yield order
  }

  def accountStateAware(
      sender: Address,
      tradableBalance: Option[AssetId] => Long,
      activeOrderCount: => Int,
      orderExists: ByteStr => Boolean,
  )(order: Order): Result[Order] =
    for {
      _ <- lift(order)
        .ensure(MatcherError.UnexpectedSender(order.sender.toAddress, sender))(_.sender.toAddress == sender)
        .ensure(MatcherError.ActiveOrdersLimitReached(MaxActiveOrders))(_ => activeOrderCount < MaxActiveOrders)
        .ensure(MatcherError.OrderDuplicate(order.id()))(o => !orderExists(o.id()))
      _ <- validateBalance(order, tradableBalance)
    } yield order

  private def lift[T](x: T): Result[T] = x.asRight[MatcherError]
  private def success: Result[Unit]    = lift(())
}
