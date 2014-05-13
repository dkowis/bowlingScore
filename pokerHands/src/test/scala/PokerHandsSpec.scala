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
        PokerHands.rankHand("2C 3H 4S 8C AH") should equal(('HighCard, List('A'), List('2', '3', '4', '8')))
      }
      it("pair") {
        PokerHands.rankHand("2C 2H 4S 8C AH") should equal(('Pair, List('2'), List('4', '8', 'A')))
      }
      it("two pairs") {
        PokerHands.rankHand("2C 2H 4C 4H AH") should equal(('TwoPair, List('2', '4'), List('A')))
      }
      it("three of a kind") {
        PokerHands.rankHand("2C 2H 2S 4H AH") should equal(('ThreeOfAKind, List('2'), List('4', 'A')))
      }
      it("straight") {
        PokerHands.rankHand("3C 4H 5S 6H 7D") should equal(('Straight, List('3','4','5','6','7'), List()))
      }
      it("flush") {
        PokerHands.rankHand("3C 4C 7C 9C QC") should equal(('Flush, List('3','4','7','9','Q'), List()))
      }
      it("full house") {
        PokerHands.rankHand("3C 3D 4C 4D 4S") should equal(('FullHouse, List('3', '4'), List()))
      }
      it("four of a kind") {
        PokerHands.rankHand("3C 3S 3D 3H 4S") should equal(('FourOfAKind, List('3'), List('4')))
      }
      it("straight flush") {
        PokerHands.rankHand("4H 5H 6H 7H 8H") should equal(('StraightFlush, List('4','5','6','7','8'), List()))
      }
    }
  }
}
