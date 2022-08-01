import java.time.LocalDate

class Ledger(val transactions: Seq[Transaction] = Seq()) {
  def deposit(amount: Int, date: LocalDate): Ledger = {
    new Ledger(
      transactions :+ new Transaction(amount, date)
    )
  }

  def withdraw(amount: Int, date: LocalDate): Ledger = {
    new Ledger(

      transactions :+ new Transaction(-amount, date)
    )
  }

  def balance: Int = {
    transactions.foldLeft(0)((runningBalance: Int, transaction: Transaction) => runningBalance + transaction.amount)
  }
}