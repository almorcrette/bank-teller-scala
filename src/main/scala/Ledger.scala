import java.time.LocalDate

class Ledger(val transactions: Seq[Transaction] = Seq())

object LedgerTools {

  def deposit(amount: Int, date: LocalDate)(ledger: Ledger = new Ledger()): Ledger = {
    new Ledger(
      ledger.transactions :+ new Transaction(amount, date)
    )
  }

  def withdraw(amount: Int, date: LocalDate)(ledger: Ledger = new Ledger()): Ledger = {
    new Ledger(
      ledger.transactions :+ new Transaction(-amount, date)
    )
  }

  def balance(ledger: Ledger): Int = {
    ledger.transactions.foldLeft(0)((runningBalance: Int, transaction: Transaction) => runningBalance + transaction.amount)
  }
}