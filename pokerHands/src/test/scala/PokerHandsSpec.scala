import org.scalatest.{FunSpec, Matchers}

class PokerHandsSpec extends FunSpec with Matchers {

  implicit val handToList:String => List[String] = {
    _.split(" ").toList
  }

  describe("comparing poker hands ") {

    val tests = Map(
      "2H 3D 5S 9C KD 2C 3H 4S 8C AH" -> 2,
      "2H 2D 3D 4S 9C 2C 3H 4S 8C AH" -> 1
    )

    tests.map(pair => {
      val hands = pair._1
      val winner = pair._2

      it(s"With $hands Player $winner wins") {
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
        PokerHands.scoreHand("2C 3H 4S 8C AH") should equal('HighCard)
      }
      it("pair") {
        PokerHands.scoreHand("2C 2H 4S 8C AH") should equal('Pair)
      }
      it("two pairs") {
        PokerHands.scoreHand("2C 2H 4C 4H AH") should equal('TwoPair)
      }
      it("three of a kind") {
        PokerHands.scoreHand("2C 2H 2S 4H AH") should equal('ThreeOfAKind)
      }
      it("straight") {
        PokerHands.scoreHand("3C 4H 5S 6H 7D") should equal('Straight)
      }
      it("flush") {
        PokerHands.scoreHand("3C 4C 7C 9C QC") should equal('Flush)
      }
      it("full house") {
        PokerHands.scoreHand("3C 3D 4C 4D 4S") should equal('FullHouse)
      }
      it("four of a kind") {
        PokerHands.scoreHand("3C 3S 3D 3H 4S") should equal('FourOfAKind)
      }
      it("straight flush") {
        PokerHands.scoreHand("4H 5H 6H 7H 8H") should equal('StraightFlush)
      }
    }
  }
}
