
object PokerHands {
  //Ranked lowest to highest
  val ranks = List('HighCard, 'Pair, 'TwoPair, 'ThreeOfAKind, 'Straight, 'Flush, 'FullHouse, 'FourOfAKind, 'StraightFlush)
  val cardValues = List('2', '3', '4', '5', '6', '7', '8', '9', 'J', 'Q', 'K', 'A')

  //TODO: create extractor objects to use in a pattern match for converting a "hand" to a Ranked hand
  // A ranked hand: ('TwoPair, '4', List('5','6','K'))
  //                 Rank    , value of that rank, remaining cards
  // For HighCard: ('HighCard, 'A', List('2','3','4','6'))

  // Then a method that takes this tuple and can compare two of them!


  def apply(cards: String) = {
    //Convert the cards to a list
    val (one, two) = hands(cards)

    //Will have to do additional magic logic with a HighCard
    val rank1 = ranks.indexOf(rankHand(one))
    val rank2 = ranks.indexOf(rankHand(two))

    if(rank1 > rank2) {
      1
    } else if(rank2 > rank1){
      2
    } else {
      //they must be the same, and it's high card of what's left...
      //RUH ROH, need to filter out the remaining card?
      0
    }
  }

  def hands(cards: String): (List[String], List[String]) = {
    val split = cards.split(" ")
    (split.slice(0, 5).toList, split.slice(5, 10).toList)
  }

  def rankHand(hand: List[String]): Symbol = {
    val straightOrder = List('A', '2', '3', '4', '5', '6', '7', '8', '9', 'J', 'Q', 'K', 'A')

    val numbersOnly = hand.map(c => c.charAt(0))
    val suitsOnly = hand.map(c => c.charAt(1))

    val flush = suitsOnly.groupBy(l => l).map(t => (t._1, t._2.length)).count(_._2 == 5) == 1
    val straight = straightOrder.containsSlice(numbersOnly)

    val valueGroups = numbersOnly.groupBy(l => l).map(t => (t._1, t._2.length))

    val pairs = valueGroups.count(t => t._2 == 2)
    val threeOfAKind = valueGroups.count(t => t._2 == 3) == 1
    val fourOfAKind = valueGroups.count(t => t._2 == 4) == 1

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
