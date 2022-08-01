import java.time.LocalDate

class Ledger(val balance: Int = 0, val transactions: Seq[Transaction] = Seq()) {
  def deposit(amount: Int, date: LocalDate): Ledger = {
    new Ledger(
      balance + amount,
      transactions :+ new Transaction(amount, date)
    )
  }

  def withdraw(amount: Int, date: LocalDate): Ledger = {
    new Ledger(
      balance - amount,
      transactions :+ new Transaction(-amount, date)
    )
  }
}