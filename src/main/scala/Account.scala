import java.time.LocalDate

class Account(private var ledger: Seq[Transaction] = Seq()) {

  def deposit(amount: Int, date: LocalDate): Unit = {
    ledger = ledger :+ new Transaction(amount, date)
  }

  def withdraw(amount: Int, date: LocalDate): Unit = {
    ledger = ledger :+ new Transaction(-amount, date)
  }

  def balance: Int = {
    ledger.foldLeft(0)((runningBalance: Int, transaction: Transaction) => runningBalance + transaction.amount)
  }
}