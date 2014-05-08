
object BowlingScore {

  //have to create an extractor to get at the proper value
  // I have to extract stuff that might be a digit or -
  object BowlingNumber {
    def unapply(value: Char): Option[Int] = {
      if (value == '-') {
        Some(0)
      } else if (value.isDigit) {
        Some(value.asDigit)
      } else {
        None
      }
    }
  }


  def apply(frames: String) = {
    def scorePins(pins: Char): Int = {
      pins match {
        case '-' => 0
        case 'X' => 10
        case x => x.asDigit
      }
    }

    def calculateScore(frameIndex: Int, score: Int, fl: List[Char]): Int = {
      if (fl.isEmpty || frameIndex == 10) {
        //If we're all done, or we hit the 10th frame
        score
      } else {

        //What constitutes a frame?
        // These are the three possible things
        fl match {
          case BowlingNumber(r1) :: BowlingNumber(r2) :: xs => {
            calculateScore(frameIndex+1, score + r1 + r2, xs)
          }
          case BowlingNumber(r1) :: '/' :: xs => {
            val spareScore = scorePins(xs.head)
            calculateScore(frameIndex+1, score + spareScore + 10, xs)
          }
          case 'X' :: xs => {
            val strikeScore = xs match {
              case BowlingNumber(ir1) :: '/' :: ixs => 10
              case ir1 :: ir2 :: ixs => scorePins(ir1) + scorePins(ir2)
            }
            calculateScore(frameIndex+1, score + strikeScore + 10, xs)
          }
        }

      }
    }

    val pinsList = frames.toList
    calculateScore(0, 0, pinsList)
  }
}
