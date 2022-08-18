package snake

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SnakeGameSpec extends AnyFlatSpec with Matchers {
  "SnakeGame" should "input sequence that eats the first pellet" in {
    // use a hardcoded seed for determinism
    val random = new scala.util.Random(0)
    val game = new SnakeGame(random, 6, 6)

    game.fieldMap shouldBe {
      """|. . . . . .
         |. @ . . . .
         |. . . o . .
         |. . . x . .
         |. . . x . .
         |. . . . . .""".stripMargin
    }

    // eat the first pallet
    game.run(Direction.Forward)
    game.run(Direction.Left)
    game.run(Direction.Forward)

    game.fieldMap shouldBe {
      """|. . . . . .
         |. o x x . .
         |@ . . x . .
         |. . . . . .
         |. . . . . .
         |. . . . . .""".stripMargin
    }

    // move one more forward
    game.run(Direction.Forward)

    game.fieldMap shouldBe {
      """|. . . . . .
         |o x x x . .
         |@ . . . . .
         |. . . . . .
         |. . . . . .
         |. . . . . .""".stripMargin
    }
  }
  
}
