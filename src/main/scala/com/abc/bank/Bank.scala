package com.abc.bank

import scala.collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}
import com.abc.bank.customer.Customer
import com.abc.bank.account.Account

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customers)
      summary = summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    summary
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    var total: Double = 0
    for (c <- customers) total += c.totalInterestEarned
    return total
  }

  def getFirstCustomer: String = {
    customers.toList match {
      case Nil => "There are no customers"
      case c :: cs => c.name
    }
  }

  def transferBetweenAccounts(customer: Customer, to: Account, from: Account, amount: Double): Unit = {
    require(0.0 < amount)
    synchronized {
      val fromTotal = from.sumTransactions()
      val amountAdj = if (amount < fromTotal) amount else fromTotal
      from.withdraw(amountAdj)
      to.deposit(amountAdj)
    }
  }

}


