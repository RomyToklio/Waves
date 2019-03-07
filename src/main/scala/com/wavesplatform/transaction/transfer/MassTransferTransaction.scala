package com.wavesplatform.transaction.transfer

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.ValidationError.Validation
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{ParsedTransfer, toJson}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject, JsValue, Json}

import scala.annotation.meta.field
import scala.util.{Either, Failure, Success, Try}

case class MassTransferTransaction private (assetId: Asset,
                                            sender: PublicKeyAccount,
                                            transfers: List[ParsedTransfer],
                                            timestamp: Long,
                                            fee: Long,
                                            attachment: Array[Byte],
                                            proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: MassTransferTransaction.type = MassTransferTransaction
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {

    val assetIdBytes = assetId.byteRepr

    val transferBytes = transfers
      .map { case ParsedTransfer(recipient, amount) => recipient.bytes.arr ++ Longs.toByteArray(amount) }
      .fold(Array())(_ ++ _)

    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      assetIdBytes,
      Shorts.toByteArray(transfers.size.toShort),
      transferBytes,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee),
      Deser.serializeArray(attachment)
    )
  }
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))

  override val assetFee: (Asset, Long) = (Waves, fee)

  override def jsonBase(): JsObject = {
    super.jsonBase() ++ Json.obj(
      "version"       -> version,
      "assetId"       -> assetId.maybeBase58Repr,
      "attachment"    -> Base58.encode(attachment),
      "transferCount" -> transfers.size,
      "totalAmount"   -> transfers.map(_.amount).sum
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj("transfers" -> toJson(transfers))
  }

  def compactJson(recipients: Set[AddressOrAlias]): JsObject =
    jsonBase() ++ Json.obj("transfers" -> toJson(transfers.filter(t => recipients.contains(t.address))))

  override def checkedAssets(): Seq[Asset] = Seq(assetId)
  override def version: Byte               = MassTransferTransaction.version
}

object MassTransferTransaction extends TransactionParserFor[MassTransferTransaction] with TransactionParser.OneVersion {

  override val typeId: Byte  = 11
  override val version: Byte = 1
  val MaxTransferCount       = 100

  @ApiModel
  case class Transfer(
      @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk", required = true) recipient: String,
      @(ApiModelProperty @field)(dataType = "long", example = "3000000000", required = true) amount: Long)

  case class ParsedTransfer(address: AddressOrAlias, amount: Long)

  implicit val transferFormat: Format[Transfer] = Json.format

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val sender           = PublicKeyAccount(bytes.slice(0, KeyLength))
      val (assetIdOpt, s0) = Deser.parseByteArrayOption(bytes, KeyLength, AssetIdLength)
      val transferCount    = Shorts.fromByteArray(bytes.slice(s0, s0 + 2))

      val assetId =
        assetIdOpt
          .map(id => IssuedAsset(ByteStr(id)))
          .getOrElse(Waves)

      def readTransfer(offset: Int): (Validation[ParsedTransfer], Int) = {
        AddressOrAlias.fromBytes(bytes, offset) match {
          case Right((addr, ofs)) =>
            val amount = Longs.fromByteArray(bytes.slice(ofs, ofs + 8))
            (Right[ValidationError, ParsedTransfer](ParsedTransfer(addr, amount)), ofs + 8)
          case Left(e) => (Left(e), offset)
        }
      }

      val transfersList: List[(Validation[ParsedTransfer], Int)] =
        List.iterate(readTransfer(s0 + 2), transferCount) { case (_, offset) => readTransfer(offset) }

      val s1 = transfersList.lastOption.map(_._2).getOrElse(s0 + 2)
      val tx: Validation[MassTransferTransaction] = for {
        transfers <- transfersList.map { case (ei, _) => ei }.sequence
        timestamp               = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
        feeAmount               = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
        (attachment, attachEnd) = Deser.parseArraySize(bytes, s1 + 16)
        proofs <- Proofs.fromBytes(bytes.drop(attachEnd))
        mtt    <- MassTransferTransaction.create(assetId, sender, transfers, timestamp, feeAmount, attachment, proofs)
      } yield mtt
      tx.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(assetId: Asset,
             sender: PublicKeyAccount,
             transfers: List[ParsedTransfer],
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    Try {
      transfers.map(_.amount).fold(feeAmount)(Math.addExact)
    }.fold(
      ex => Left(ValidationError.OverflowError),
      totalAmount =>
        if (transfers.lengthCompare(MaxTransferCount) > 0) {
          Left(ValidationError.GenericError(s"Number of transfers ${transfers.length} is greater than $MaxTransferCount"))
        } else if (transfers.exists(_.amount < 0)) {
          Left(ValidationError.GenericError("One of the transfers has negative amount"))
        } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
          Left(ValidationError.TooBigArray)
        } else if (feeAmount <= 0) {
          Left(ValidationError.InsufficientFee())
        } else {
          Right(MassTransferTransaction(assetId, sender, transfers, timestamp, feeAmount, attachment, proofs))
      }
    )
  }

  def signed(assetId: Asset,
             sender: PublicKeyAccount,
             transfers: List[ParsedTransfer],
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(assetId, sender, transfers, timestamp, feeAmount, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(assetId: Asset,
                 sender: PrivateKeyAccount,
                 transfers: List[ParsedTransfer],
                 timestamp: Long,
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(assetId, sender, transfers, timestamp, feeAmount, attachment, sender)
  }

  def parseTransfersList(transfers: List[Transfer]): Validation[List[ParsedTransfer]] = {
    transfers.traverse {
      case Transfer(recipient, amount) =>
        AddressOrAlias.fromString(recipient).map(ParsedTransfer(_, amount))
    }
  }

  private def toJson(transfers: List[ParsedTransfer]): JsValue = {
    Json.toJson(transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) })
  }
}
