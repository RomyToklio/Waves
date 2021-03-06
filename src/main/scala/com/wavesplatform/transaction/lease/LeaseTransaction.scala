package com.wavesplatform.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{Address, AddressOrAlias, PublicKeyAccount}
import com.wavesplatform.transaction.{AssetId, ProvenTransaction, ValidationError, VersionedTransaction}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

trait LeaseTransaction extends ProvenTransaction with VersionedTransaction {
  def amount: Long
  def fee: Long
  def recipient: AddressOrAlias
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"   -> version,
      "amount"    -> amount,
      "recipient" -> recipient.stringRepr,
      "fee"       -> fee,
      "timestamp" -> timestamp
    ))

  protected final val bytesBase = Coeval.evalOnce(
    Bytes.concat(sender.publicKey, recipient.bytes.arr, Longs.toByteArray(amount), Longs.toByteArray(fee), Longs.toByteArray(timestamp)))
}

object LeaseTransaction {

  val typeId: Byte = 8

  object Status {
    val Active   = "active"
    val Canceled = "canceled"
  }

  def validateLeaseParams(tx: LeaseTransaction): Either[ValidationError, Unit] = {
    validateLeaseParams(tx.amount, tx.fee, tx.recipient, tx.sender)
  }

  def validateLeaseParams(amount: Long, fee: Long, recipient: AddressOrAlias, sender: PublicKeyAccount): Either[ValidationError, Unit] =
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount(amount, "waves"))
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else if (recipient.isInstanceOf[Address] && sender.stringRepr == recipient.stringRepr) {
      Left(ValidationError.ToSelf)
    } else Right(())
}
