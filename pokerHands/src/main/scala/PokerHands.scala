
object PokerHands {

  def apply(cards: String) = {
    //Convert the cards to a list
    hands(cards)
  }

  def hands(cards: String): (List[String], List[String]) = {
    val split = cards.split(" ")
    (split.slice(0, 5).toList, split.slice(5, 10).toList)
  }

  def scoreHand(hand: List[String]): Symbol = {
    val ordered = List('A', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'J', 'Q', 'K')
    val numbersOnly = hand.map(c => c.charAt(0))
    val suitsOnly = hand.map(c => c.charAt(1))
    val flush = suitsOnly.groupBy(l => l).map(t => (t._1, t._2.length)).count(_._2 == 5) == 1

    val straight = ordered.containsSlice(numbersOnly)

    val groups = numbersOnly.groupBy(l => l).map(t => (t._1, t._2.length))
    val pairs = groups.count(t => t._2 == 2)
    val threeOfAKind = groups.count(t => t._2 == 3) == 1
    val fourOfAKind = groups.count(t => t._2 == 4) == 1

    if (flush && straight) {
      'StraightFlush
    } else if (flush) {
      'Flush
    } else if (pairs == 1 && threeOfAKind) {
      'FullHouse
    } else if (fourOfAKind) {
      'FourOfAKind
    } else if (pairs == 1) {
      'Pair
    } else if (pairs == 2) {
      'TwoPair
    } else if (threeOfAKind) {
      'ThreeOfAKind
    } else if (straight) {
      'Straight
    } else {
      'HighCard
    }
  }
}
