
object PokerHands {

  def apply(cards: String) = {
    //Convert the cards to a list
    hands(cards)
  }

  def hands(cards:String):(List[String], List[String]) = {
    val split = cards.split(" ")
    (split.slice(0, 5).toList, split.slice(5,10).toList)
  }
}
