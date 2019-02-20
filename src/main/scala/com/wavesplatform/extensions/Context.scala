package com.wavesplatform.extensions

import akka.actor.ActorSystem
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable

trait Context {
  def settings: WavesSettings
  def blockchain: Blockchain
  def time: Time
  def wallet: Wallet
  def balanceChanges: Observable[(Address, Option[ByteStr])]
  def spendableBalance(address: Address, assetId: Option[ByteStr]): Long
  def addToUtx(tx: Transaction): Unit
  def actorSystem: ActorSystem
}
