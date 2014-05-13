object PokerHands {
  //Ranked lowest to highest
  val ranks = List('HighCard, 'Pair, 'TwoPair, 'ThreeOfAKind, 'Straight, 'Flush, 'FullHouse, 'FourOfAKind, 'StraightFlush)
  val cardValues = List('2', '3', '4', '5', '6', '7', '8', '9', 'J', 'Q', 'K', 'A')

  //Extractors made it overly complicated, because I duplicated so much data....
  // A ranked hand: ('TwoPair, List('4','5'), List('6','K'))
  //                 Rank    , value of that rank, remaining cards
  // For HighCard: ('HighCard, List('A'), List('2','3','4','6'))

  // Then a method that takes this tuple and can compare two of them!

  type HandRank = (Symbol, List[Char], List[Char])
  val straightOrder = List('A', '2', '3', '4', '5', '6', '7', '8', '9', 'J', 'Q', 'K', 'A')

  def apply(cards: String) = {
    //Convert the cards to a list
    val (one, two) = hands(cards)

    compareHands(rankHand(one), rankHand(two)) match {
      case -1 => 2
      case 0 => 0
      case 1 => 1
    }
  }

  def hands(cards: String): (List[String], List[String]) = {
    val split = cards.split(" ")
    (split.slice(0, 5).toList, split.slice(5, 10).toList)
  }

  //Follows the compareTo logic
  def compareHands(hand1:HandRank, hand2:HandRank):Int = {
    def listCompare(list1:List[Char], list2:List[Char]):Int = {
      if(list1.isEmpty || list2.isEmpty) {
        0 //they're the same!
      } else {
        val comparison = cardValues.indexOf(list1.head).compareTo(cardValues.indexOf(list2.head))
        if(comparison == 0) {
          //They're the same, iterate!
          listCompare(list1.tail, list2.tail)
        } else {
          comparison
        }
      }
    }
    if(hand1._1 == hand2._1) {
      //The hands are the same rank, compare the subranks
      if(hand1._2 == hand2._2) {
        //the subranks are the same, need to loop through the final cards
        listCompare(hand1._3, hand2._3)
      } else {
        //Loop through the list of subranks
        listCompare(hand1._2, hand2._2)
      }
    } else {
      ranks.indexOf(hand1._1).compareTo(ranks.indexOf(hand2._1)) match {
        case -1 => -1
        case 0 => 0
        case 1 => 1
      }
    }
  }

  def rankHand(hand: List[String]): HandRank = {

    val numbersOnly = hand.map(c => c.charAt(0))
    val suitsOnly = hand.map(c => c.charAt(1))

    val flush = suitsOnly.groupBy(l => l).map(t => (t._1, t._2.length)).count(_._2 == 5) == 1
    val straight = straightOrder.containsSlice(numbersOnly)

    val valueGroups = numbersOnly.groupBy(l => l).map(t => (t._1, t._2.length))

    val pairs = valueGroups.count(t => t._2 == 2)
    val threeOfAKind = valueGroups.count(t => t._2 == 3) == 1
    val fourOfAKind = valueGroups.count(t => t._2 == 4) == 1

    if (flush && straight) {
      ('StraightFlush, numbersOnly, List())
    } else if (flush) {
      ('Flush, numbersOnly, List())
    } else if (pairs == 1 && threeOfAKind) {
      ('FullHouse, valueGroups.filter(t => t._2 == 2).keys.toList ++ valueGroups.filter(t => t._2 == 3).keys, List())
    } else if (fourOfAKind) {
      ('FourOfAKind, valueGroups.filter(t => t._2 == 4).keys.toList, valueGroups.filterNot(t => t._2 == 4).keys.toList)
    } else if (pairs == 1) {
      ('Pair, valueGroups.filter(t => t._2 == 2).keys.toList, numbersOnly.intersect(valueGroups.filterNot(t => t._2 == 2).keys.toList))
    } else if (pairs == 2) {
      ('TwoPair, valueGroups.filter(t => t._2 == 2).keys.toList, numbersOnly.filter(i => valueGroups.filterNot(t => t._2 == 2).keys.toList.contains(i)))
    } else if (threeOfAKind) {
      ('ThreeOfAKind, valueGroups.filter(t => t._2 == 3).keys.toList, numbersOnly.intersect(valueGroups.filterNot(t => t._2 == 3).keys.toList))
    } else if (straight) {
      ('Straight, numbersOnly, List())
    } else {
      ('HighCard, List(numbersOnly.last), numbersOnly.slice(0,4))
    }
  }
}
