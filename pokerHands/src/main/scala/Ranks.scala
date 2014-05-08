import java.lang.Comparable

/**
 * Created by dkowis on 5/7/14.
 */
object Ranks {

  def apply(cards:List[String]):Hand = {
    //TODO: turn a list of cards into one of the subclasses of Hand
    ???
  }

  trait Hand extends Comparable[Hand] {
    def compareTo(o: Hand): Int = {
      val tf: (Hand => Int) = selfMatch orElse {
        case _ => this.index.compareTo(o.index)
      }
      tf(o)
    }

    def selfMatch: PartialFunction[Hand, Int]

    def index: Int
  }

  case class HighCard(cards: List[String]) extends Hand {
    override val index = 0

    override def selfMatch: PartialFunction[Hand, Int] = {
      case x: HighCard => {
        println("Card comparison")
        ???
      }
    }
  }

  case class Pair(cards: List[String]) extends Hand {
    override val index: Int = 1

    override def selfMatch: PartialFunction[Hand, Int] = {
      case x:Pair => {
        println("CHECKING PAIRS")
        ???
      }
    }
  }

  case class TwoPair(cards: List[String]) extends Hand {
    override val index: Int = 2

    override def selfMatch: PartialFunction[Hand, Int] = ???
  }

  case class ThreeOfAKind(cards: List[String]) extends Hand {
    override val index: Int = 3

    override def selfMatch: PartialFunction[Hand, Int] = ???
  }

  case class Straight(cards: List[String]) extends Hand {
    override val index: Int = 4

    override def selfMatch: PartialFunction[Hand, Int] = ???
  }

  case class Flush(cards: List[String]) extends Hand {
    override val index: Int = 5

    override def selfMatch: PartialFunction[Hand, Int] = ???
  }

  case class FullHouse(cards: List[String]) extends Hand {
    override val index: Int = 6

    override def selfMatch: PartialFunction[Hand, Int] = ???
  }

  case class FourOfAKind(cards: List[String]) extends Hand {
    override val index: Int = 7

    override def selfMatch: PartialFunction[Hand, Int] = ???
  }

  case class StraightFlush(cards: List[String]) extends Hand {
    override val index: Int = 8

    override def selfMatch: PartialFunction[Hand, Int] = ???
  }

}
