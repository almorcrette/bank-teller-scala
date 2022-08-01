import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class LedgerSpec extends AnyWordSpec with Matchers {
  val Day1: LocalDate = LocalDate.EPOCH
  val Day2: LocalDate = Day1.plusDays(1)

  "A Bank Ledger" should {
    "deposit once and retrieve a balance" in {
      val ledger = new Ledger()
        .deposit(1000, Day1)
      ledger.balance shouldEqual 1000
    }
    "deposit multiple times and retrieve a balance" in {
      val ledger = new Ledger()
        .deposit(1000, Day1)
        .deposit(2000, Day2)
      ledger.balance shouldEqual 3000
    }
    "deposit and withdraw and maintain correct balance" in {
      val ledger = new Ledger()
        .deposit(1000, Day1)
        .withdraw(499, Day2)
      ledger.balance shouldEqual 501
    }
    "deposit and withdraw and maintain correct balance at each transaction" in {
      val ledgerToday = new Ledger()
        .deposit(1000, Day1)
      val ledgerTomorrow = ledgerToday
        .withdraw(499, Day2)
      ledgerToday.balance shouldEqual 1000
      ledgerTomorrow.balance shouldEqual 501
    }
    "deposit and withdraw and log transactions" in {
      val ledger = new Ledger()
        .deposit(1000, Day1)
        .withdraw(499, Day2)
      ledger.transactions(0).amount shouldEqual 1000
      ledger.transactions(0).date shouldEqual Day1
      ledger.transactions(1).amount shouldEqual -499
      ledger.transactions(1).date shouldEqual Day2
    }
  }
}