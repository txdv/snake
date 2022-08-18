package snake

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HelloSpec extends AnyFlatSpec with Matchers {
  "Main object" should "return unit" in {
    //Main.main(Array()) shouldEqual ()
  }

  "abs" should "be 1 when height" in {
    /*
    Pos(0, -1).abs(5, 5) shouldBe(Pos(0, 4))
    Pos(0,  5).abs(5, 5) shouldBe(Pos(0, 0))

    Pos.distance(5, 0)(5) shouldBe(0)
    Pos.distance(0, 4)(5) shouldBe(1)
    */

    Pos.positiveMod(-1, 5) shouldBe(4)
    Pos.positiveMod(-10, 5) shouldBe(0)
    Pos.positiveMod(5, 5) shouldBe(0)
  }
}
