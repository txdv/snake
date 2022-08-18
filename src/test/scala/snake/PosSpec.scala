package snake

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PosSpec extends AnyFlatSpec with Matchers {
  "positiveMod" should "should return the positive mod of a number" in {
    Pos.positiveMod(-1, 5) shouldBe(4)
    Pos.positiveMod(-10, 5) shouldBe(0)
    Pos.positiveMod(5, 5) shouldBe(0)
    Pos.positiveMod(7, 5) shouldBe(2)
  }

  "positiveMod" should "return the positive mod of each element in the position" in {
    Pos(-1, 5).positiveMod(5, 5) shouldBe(Pos(4, 0))
    Pos(5, 7).positiveMod(5, 5) shouldBe(Pos(0, 2))
  }

  "multiplication" should "multiply numbers" in {
    Pos(0, 0) * 1 shouldBe(Pos(0, 0))
    Pos(1, 1) * 1 shouldBe(Pos(1, 1))
    Pos(2, 3) * 4 shouldBe(Pos(8, 12))
    Pos(2, 3) * -1 shouldBe(Pos(-2, -3))
  }

  "minus" should "substract each element in a Pos" in {
    Pos(0, 0) - Pos(1, 1) shouldBe(Pos(-1, -1))
    Pos(1, 2) - Pos(10, 20) shouldBe(Pos(-9, -18))
  }

  "plus" should "add each element in a Pos" in {
    Pos(1, 2) + Pos(10, 20) shouldBe(Pos(11, 22))
    Pos(0, 0) + Pos(-10, -20) shouldBe(Pos(-10, -20))
  }

  "asterix" should "multiply with a constant" in {
    Pos(0, 1) * 2 shouldBe(Pos(0, 2))
    Pos(2, -10) * -2 shouldBe(Pos(-4, 20))
  }

  "swap" should "swap axis in Pos" in {
    Pos(0, 0).swap shouldBe(Pos(0, 0))
    Pos(0, 1).swap shouldBe(Pos(1, 0))
    Pos(-10, 20).swap shouldBe(Pos(20, -10))
  }
}
