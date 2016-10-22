package com.abc.bank.account

package object AccountUtil {
  sealed trait AccountType
  case object CHECKING extends AccountType
  case object SAVINGS extends AccountType
  case object MAXI_SAVINGS extends AccountType
  val MAXI_SAVINGS_PENALTY_DAYS = 10
}
import AccountUtil._
import com.abc.common.CommnonUtil._
import com.abc.common.Transaction
import com.abc.common._
import scala.collection.mutable.ListBuffer

case class Account(accountType: AccountType, transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()

    accountType match {
      case SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case MAXI_SAVINGS =>
        val cal = DateProvider.getInstance
        val msCutoffTime = cal.now.getTime -(MAXI_SAVINGS_PENALTY_DAYS*ONE_DAY)
        val maxi_savings_penalty = !transactions.find( e => {
          e.amount < 0 && (msCutoffTime < e.transactionDate.getTime)
        }).isEmpty
        if (amount <= 1000)
          amount * 0.02
        else if (amount <= 2000) {
          if (maxi_savings_penalty)
            20 + (amount - 1000) * 0.05
          else
            20 + (amount - 1000) * 0.001
        }
        else
          70 + (amount - 2000) * 0.1
      case CHECKING =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}