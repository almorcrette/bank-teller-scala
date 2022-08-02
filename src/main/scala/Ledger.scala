import java.time.LocalDate

class Ledger(val transactions: Seq[Transaction] = Seq())

object LedgerTools {

  def deposit(amount: Double, date: LocalDate)(ledger: Ledger = new Ledger()): Ledger = {
    new Ledger(
      ledger.transactions :+ new Transaction(amount, date)
    )
  }

  def withdraw(amount: Double, date: LocalDate)(ledger: Ledger = new Ledger()): Ledger = {
    new Ledger(
      ledger.transactions :+ new Transaction(-amount, date)
    )
  }

  def balance(ledger: Ledger): Double = {
    ledger.transactions.foldLeft(0.0)((runningBalance: Double, transaction: Transaction) => runningBalance + transaction.amount)
  }
}