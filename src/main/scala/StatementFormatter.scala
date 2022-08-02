import scala.annotation.tailrec
import scala.collection.mutable

object StatementFormatter {

  def format: Seq[Transaction] => String =
    sortTransactions _ andThen (
      formatTransactions _ andThen
        addHeader
      )

  private def sortTransactions(transactions: Seq[Transaction]) =
    transactions.sortBy(tx => tx.date)


  private def formatTransactions(transactions: Seq[Transaction]): String = {
    val transactionsReversed = transactions.reverse

    @tailrec def recursiveFormatter(
                                     statementBuilder: mutable.StringBuilder,
                                     startingBalance: Double,
                                     transactionsCountDown: Int
                                   ): String = {
      if (transactionsCountDown == 0) statementBuilder.toString
      else {
        val currentTransaction = transactionsReversed(transactionsCountDown - 1)
        recursiveFormatter(
          addToStatement(statementBuilder, startingBalance, currentTransaction),
          startingBalance + currentTransaction.amount,
          transactionsCountDown - 1)
      }
    }

    recursiveFormatter(new mutable.StringBuilder(), 0.0, transactions.length)
  }

  private def addToStatement(
                              statementBuilder: mutable.StringBuilder,
                              startingBalance: Double,
                              currentTransaction: Transaction
                            ): mutable.StringBuilder = {
    statementBuilder.addString(
      new mutable.StringBuilder(
        formatLine(
          twoDPs(currentTransaction.amount),
          currentTransaction.date,
          twoDPs(startingBalance + currentTransaction.amount)
        )
      )
    )
  }

  private def formatLine(a: Any, b: Any, c: Any) = s"$a,$b,$c\n"

  private def addHeader(formattedTxns: String): String = {
    formatLine("Amount", "Date", "Balance").concat(formattedTxns)
  }

  private def twoDPs(amount: Double): String = {
    "%1.2f".format(amount)
  }


}