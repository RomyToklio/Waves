package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.validation._
import com.wavesplatform.transaction.{AssetId, ProvenTransaction, ValidationError, _}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait ReissueTransaction extends ProvenTransaction with VersionedTransaction {
  def assetId: ByteStr
  def quantity: Long
  def reissuable: Boolean
  def fee: Long
  def chainByte: Option[Byte]

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "chainId"    -> chainByte,
      "assetId"    -> assetId.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    ))

  protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
  override def checkedAssets(): Seq[AssetId] = Seq(assetId)
}

object ReissueTransaction {

  val typeId: Byte = 5

  def validateReissueParams(tx: ReissueTransaction): Either[ValidationError, Unit] = {
    validateReissueParams(tx.quantity, tx.fee)
  }

  def validateReissueParams(quantity: Long, fee: Long): Either[ValidationError, Unit] =
    (validateAmount(quantity, "assets"), validateFee(fee))
      .mapN { case _ => () }
      .leftMap(_.head)
      .toEither
}
