package com.abc.test

import com.abc.bank.Bank
import com.abc.bank.account.Account
import com.abc.common.Transaction
import com.abc.bank.account.AccountUtil.{CHECKING, SAVINGS, MAXI_SAVINGS}
import com.abc.bank.customer.Customer
import org.scalatest.{FlatSpec, Matchers}


class TransactionTest extends FlatSpec with Matchers {

  "Transaction" should "type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }


  it should "calculate interestEarned no penalty" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
    val maxiAccount: Account = new Account(MAXI_SAVINGS)
    bill.openAccount(maxiAccount)
    bank.addCustomer(bill)
    maxiAccount.deposit(10000.0)
    maxiAccount.interestEarned should be(870.00)
  }

  it should "calculate interestEarned with penalty" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
    val maxiAccount: Account = new Account(MAXI_SAVINGS)
    bill.openAccount(maxiAccount)
    bank.addCustomer(bill)
    maxiAccount.deposit(10000.0)
    maxiAccount.withdraw(1000.0)
    maxiAccount.interestEarned should be (770.0)
  }

}
