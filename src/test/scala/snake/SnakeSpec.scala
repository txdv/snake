package snake

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SnakeSpec extends AnyFlatSpec with Matchers {
  "move" should "add new head without droping tail" in {
    val snake = Snake(Pos(2, 2), Pos(2, 3))

    val result = snake.move(Pos(2, 1), tailDrop = false)

    result.positions(10, 10) shouldBe(Seq(Pos(2, 1), Pos(2, 2), Pos(2, 3)))
  }

  "move" should "add new head with dropping tail" in {
    val snake = Snake(Pos(2, 2), Pos(2, 3))

    val result = snake.move(Pos(2, 1), tailDrop = true)

    result.positions(10, 10) shouldBe(Seq(Pos(2, 1), Pos(2, 2)))
  }

  "isOverlapping" should "return false if snake does fit in box" in {
    val snake = Snake(Pos(0, 0), Pos(1, 0))

    snake.isOverlapping(2, 2) shouldBe(false)
  }

  "isOverlapping" should "return false if snake fits in box but with outside position" in {
    val snake = Snake(Pos(-10, -10), Pos(-9, -10))

    snake.isOverlapping(2, 2) shouldBe(false)
  }

  "isOverlapping" should "return true if snake does not fix in box" in {
    val snake = Snake(Pos(0, 0), Pos(1, 0), Pos(2, 0))

    snake.isOverlapping(2, 2) shouldBe(true)
  }

  "isOverlapping" should "return true if snake runs into itself" in {
    val snake = Snake(Pos(0, 0), Pos(1, 0), Pos(1, 1), Pos(0, 1), Pos(0, 0))

    snake.isOverlapping(10, 10) shouldBe(true)
  }

  "positions" should "return the positions of the snake" in {
    Snake(Pos(0, 0), Pos(1, 0)).positions(2, 2) shouldBe {
      Seq(Pos(0, 0), Pos(1, 0))
    }

    // it folds the snake and has overlapping positions
    Snake(Pos(0, 0), Pos(1, 0), Pos(2, 0)).positions(2, 2) shouldBe {
      Seq(Pos(0, 0), Pos(1, 0), Pos(0, 0))
    }
  }

}