import scala.annotation.tailrec
import scala.collection.mutable

object StatementFormatter {
  val StatementHeader: String = formatLine("Amount", "Date", "Balance")

  def format: Seq[Transaction] => String =
    sortTransactions _ andThen(
      formatTransactions _ andThen
        addHeader
    )

  private def sortTransactions(txns: Seq[Transaction]) =
    txns.sortBy(tx => tx.date)


  private def formatTransactions(transactions: Seq[Transaction]): String = {
    val transactionsReversed = transactions.reverse
    @tailrec def recursiveFormatter(
                                     statementBuilder: mutable.StringBuilder,
                                     startingBalance: Int,
                                     transactionsCountDown: Int
                                   ): String = {
      if (transactionsCountDown == 0) statementBuilder.toString
      else {
        val currentTransaction = transactionsReversed(transactionsCountDown - 1)
        recursiveFormatter(
          statementAddition(statementBuilder, startingBalance, currentTransaction),
          startingBalance + currentTransaction.amount,
          transactionsCountDown - 1)
      }
    }
    recursiveFormatter(new mutable.StringBuilder(), 0, transactions.length)
  }

  private def statementAddition(statementBuilder: mutable.StringBuilder, startingBalance: Int, currentTransaction: Transaction): StringBuilder = {
    statementBuilder.addString(
      new mutable.StringBuilder(
        formatLine(currentTransaction.amount, currentTransaction.date, startingBalance + currentTransaction.amount)
      )
    )
  }

  private def addHeader(formattedTxns: String): String = {
    formatLine("Amount", "Date", "Balance").concat(formattedTxns)
  }

  private def formatLine(a: Any, b: Any, c: Any) = s"$a,$b,$c\n"
}