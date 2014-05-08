import org.scalatest.{FunSpec, Matchers}

class PokerHandsSpec extends FunSpec with Matchers {

  describe("comparing poker hands ") {

    val tests = Map(
      "2H 3D 5S 9C KD 2C 3H 4S 8C AH" -> 2,
      "2H 2D 3D 4S 9C 2C 3H 4S 8C AH" -> 1
    )

    tests.map(pair => {
      val hands = pair._1
      val winner = pair._2

      it(s"With ${hands} Player ${winner} wins") {
        PokerHands(hands) should equal(winner)
      }
    })
  }

  describe("Incremental parts of comparing hands") {
    it("can split a list of cards into two hands") {
      PokerHands.hands("2H 3D 5S 9C KD 2C 3H 4S 8C AH") should equal((List("2H", "3D", "5S", "9C", "KD"), List("2C", "3H", "4S", "8C", "AH")))
    }

    describe("can tell me if a hand is a") {
      it("high card") {
        pending
      }
      it("pair") {
        pending
      }
      it("two pairs") {
        pending
      }

      it("three of a kind") {
        pending
      }
      it("straight") {
        pending
      }
      it("flush") {
        pending
      }
      it("full house") {
        pending
      }
      it("four of a kind") {
        pending
      }
      it("straight flush") {
        pending
      }
    }
  }
}
