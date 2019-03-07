package com.wavesplatform.transaction.transfer

import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.transaction._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

case class TransferTransactionV1 private (assetId: Asset,
                                          sender: PublicKeyAccount,
                                          recipient: AddressOrAlias,
                                          amount: Long,
                                          timestamp: Long,
                                          feeAssetId: Asset,
                                          fee: Long,
                                          attachment: Array[Byte],
                                          signature: ByteStr)
    extends TransferTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: TransactionParser     = TransferTransactionV1
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId) ++ bytesBase())
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))
  override val version: Byte                  = 1: Byte
}

object TransferTransactionV1 extends TransactionParserFor[TransferTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = TransferTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")

      (for {
        parsed <- TransferTransaction.parseBase(bytes, SignatureLength + 1)
        (sender, assetId, feeAssetId, timestamp, amount, feeAmount, recipient, attachment, _) = parsed
        tt <- TransferTransactionV1.create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, signature)
      } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(assetId: Asset,
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Asset,
             feeAmount: Long,
             attachment: Array[Byte],
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    TransferTransaction
      .validate(amount, assetId, feeAmount, feeAssetId, attachment)
      .map(_ => TransferTransactionV1(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, signature))
  }

  def signed(assetId: Asset,
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Asset,
             feeAmount: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(assetId: Asset,
                 sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 feeAssetId: Asset,
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, sender)
  }
}
