import org.scalatest.{FunSpec, Matchers}

class BowlingScoreSpec extends FunSpec with Matchers {

  describe("Calculating a bowling score of ") {

    val tests = Map(
      "--------------------" -> 0,
      "-1-1-1-1-1-1-1-1-1-1" -> 10,
      "-1-2-3-4-5-6-7-8-944" -> 53,
      "9-8-7-6-5-4-3-2-1---" -> 45,
      "XXXXXXXXXXXX" -> 300,
      "3423114/12XX-99---" -> 85,
      "----XXX439/4/128/4" -> 120,
      "-72/4/XXX23459/XXX" -> 172,
      "-/X1/249-8/XX--XXX" -> 147
    )

    tests.map(pair => {
      val string = pair._1
      val result = pair._2

      it(s"${string} as ${result}") {
        BowlingScore(string) should equal(result)
      }
    })
  }
}
