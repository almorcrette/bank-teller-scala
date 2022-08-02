import java.time.LocalDate

class Account(private var transactionLog: Seq[Transaction] = Seq()) {

  def deposit(amount: Int, date: LocalDate): Unit = {
    transactionLog = transactionLog :+ new Transaction(amount, date)
  }

  def withdraw(amount: Int, date: LocalDate): Unit = {
    transactionLog = transactionLog :+ new Transaction(-amount, date)
  }

  def balance: Int = {
    transactionLog.foldLeft(0)((runningBalance: Int, transaction: Transaction) => runningBalance + transaction.amount)
  }
}