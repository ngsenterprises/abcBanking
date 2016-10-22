package com.abc.test

import com.abc.bank.account.Account
import com.abc.bank.account.AccountUtil._
import com.abc.bank._

import com.abc.bank.customer.Customer
import org.scalatest.{FlatSpec, Matchers}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }

  it should "first customer when no customers exist test" in {
    val bank: Bank = new Bank
    bank.getFirstCustomer should be("There are no customers")
  }

  it should "first customer when one customer exists test" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    bank.getFirstCustomer should be("Bill")
  }

  it should "first customer when two customers exists test" in {
    val bank: Bank = new Bank
    val checkingAccount1: Account = new Account(CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount1)
    bank.addCustomer(bill)
    val checkingAccount2: Account = new Account(CHECKING)
    val Jane: Customer = new Customer("Jane").openAccount(checkingAccount2)
    bank.addCustomer(Jane)
    bank.getFirstCustomer should be("Bill")
  }

  it should "customer transer between accounts std test" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
    val checkingAccount: Account = new Account(CHECKING)
    val savingsAccount: Account = new Account(SAVINGS)
    bill.openAccount(checkingAccount)
    bill.openAccount(savingsAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(1500.0)
    savingsAccount.deposit(2000.0)
    bank.transferBetweenAccounts(bill, savingsAccount, checkingAccount, 500.00)

    val stmt =
    "Statement for Bill\n\n" +
    "Checking Account\n" +
    "  deposit $1500.00\n" +
    "  withdrawal $500.00\n" +
    "Total $1000.00\n\n" +
    "Savings Account\n" +
    "  deposit $2000.00\n" +
    "  deposit $500.00\n" +
    "Total $2500.00\n\n" +
    "Total In All Accounts $3500.00"

    bill.getStatement should be(stmt)
  }


  it should "customer transer between accounts truncated amount test" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
    val checkingAccount: Account = new Account(CHECKING)
    val savingsAccount: Account = new Account(SAVINGS)
    bill.openAccount(checkingAccount)
    bill.openAccount(savingsAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(500.0)
    savingsAccount.deposit(2000.0)
    bank.transferBetweenAccounts(bill, savingsAccount, checkingAccount, 600.00)
    val stmt =
    "Statement for Bill\n\n" +
    "Checking Account\n" +
    "  deposit $500.00\n" +
    "  withdrawal $500.00\n" +
    "Total $0.00\n\n" +
    "Savings Account\n" +
    "  deposit $2000.00\n" +
    "  deposit $500.00\n" +
    "Total $2500.00\n\n" +
    "Total In All Accounts $2500.00"

    bill.getStatement should be(stmt)
  }


  it should "customer transer between accounts zero amount test" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
    val checkingAccount: Account = new Account(CHECKING)
    val savingsAccount: Account = new Account(SAVINGS)
    bill.openAccount(checkingAccount)
    bill.openAccount(savingsAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(500.0)
    savingsAccount.deposit(2000.0)

    val stmt = "Statement for Bill\n\n" +
               "Checking Account\n" +
               "  deposit $500.00" +
               "Total $5.00\n\n" +
               "Savings Account\n" +
               "  deposit $2000.00" +
               "Total $2000.00\n\n" +
               "Total In All Accounts $2500.00"

    the [IllegalArgumentException] thrownBy(bank.transferBetweenAccounts(bill, savingsAccount, checkingAccount, 0.00)) should have message "requirement failed"
  }

}
