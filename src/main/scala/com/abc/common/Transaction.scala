package com.abc.common

case class Transaction(var amount: Double) {
  val transactionDate = DateProvider.getInstance.now
}

