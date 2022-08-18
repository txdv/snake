package snake

sealed trait Direction
object Direction {
  case object Left extends Direction
  case object Right extends Direction
  case object Forward extends Direction
}

case class Pos(x: Int, y: Int) {
  def -(other: Pos): Pos =
    this + (other * -1)
  
  def +(other: Pos): Pos =
    Pos(x + other.x, y + other.y)

  def *(m: Int): Pos =
    Pos(x * m, y * m)

  def swap: Pos =
    Pos(y, x)

  override def toString(): String = {
    s"(x: $x, y: $y)"
  }

  def positiveMod(w: Int, h: Int) = Pos(
    Pos.positiveMod(x, w),
    Pos.positiveMod(y, h),
  )
}

object Pos {
  def positiveMod(i: Int, n: Int): Int = {
    (i % n + n) % n
  }
}

case class Snake(head: Pos, tail: List[Pos]) {

  def direction: Pos =
    tail.head - head
    
  def addHead(newHead: Pos): Snake =
    Snake(newHead, head +: tail)

  def dropTail: Snake =
    Snake(head, tail.dropRight(1))

  def move(newHead: Pos, tailDrop: Boolean): Snake =
    if (tailDrop) {
      addHead(newHead).dropTail
    } else {
      addHead(newHead)
    }

  // the snake exists in a dimension with no boundaries
  // and is folded into a box to see if there are
  // overlapping values
  def isOverlapping(width: Int, height: Int): Boolean =
    positions(width, height)
      .groupBy(identity)
      .map(_._2.length)
      .max > 1

  def positions(w: Int, h: Int): List[Pos] =
    (head +: tail).map(_.positiveMod(w, h))
}

case object Snake {
  def apply(head: Pos, tail: Pos*): Snake = {
    Snake(head, tail.toList)
  }
}

case class GameState(snake: Snake, apple: Pos) {
  def display(w: Int, h: Int): Map[Pos, Display] = {
    Map(
      apple -> Display.Apple,
      snake.head.positiveMod(w, h) -> Display.Head,
    ) ++ snake.tail.map(_.positiveMod(w, h) -> Display.Tail)
  }
}

sealed trait Display {
  def char: Char = this match {
    case Display.Head => 'o'
    case Display.Tail => 'x'
    case Display.Apple => '@'
  }
}

object Display {
  case object Head extends Display
  case object Tail extends Display
  case object Apple extends Display
}

class SnakeGame(width: Int, height: Int) {

  // starting state of the game
  var state = GameState(
    Snake(Pos(3, 2), Pos(3, 3), Pos(3, 4)),
    Pos(1, 1)
  )

  def swayHead(headDirection: Pos, direction: Direction): Pos = {
    direction match {
      case Direction.Forward =>
        headDirection
      case Direction.Left =>
        headDirection.swap
      case Direction.Right =>
        headDirection.swap * -1
    }
  }

  val random = new scala.util.Random()

  def step(state: GameState, direction: Direction): GameState = {
    val snake = state.snake
    val apple = state.apple

    val nextHead = state.snake.head - swayHead(snake.direction, direction)
    
    val hitApple = nextHead.positiveMod(width, height) == state.apple

    val nextSnake = state.snake.move(nextHead, !hitApple)

    val freeFields =
      fields.flatten.toSet diff state.snake.positions(width, height).toSet

    val nextApple =
      if (hitApple) freeFields.toSeq(random.nextInt(freeFields.size))
      else state.apple

    GameState(nextSnake, nextApple)
  }

  private val fields: Seq[Seq[Pos]] = {
    (0 until height).map { y =>
      (0 until width).map { x => Pos(x, y) }
    }
  }

  def fieldMap: String = {
    val symbols = state.display(width, height)

    fields.map { row =>
      val line = row.map { pos =>
        symbols.get(pos).map(_.char).getOrElse('.')
      }

      line.mkString(" ")
    }.mkString("\n")
  }

  def run(steps: Stream[Direction]): Unit = {
    steps.foreach(run)
  }

  def run(direction: Direction): Boolean = {
    if (state.snake.isOverlapping(width, height)) {
      println(s"Game over, your snake was ${state.snake.positions(width, height).length - 1} long")
      System.exit(0)
    }

    println(EscapeCode.right(12) + EscapeCode.up(7))

    state = step(state, direction)

    println(fieldMap)
    true
  }
}

object EscapeCode {
  def right(n: Int) = s"\u001b[${n}C"
  def up(n: Int) = s"\u001b[${n}A"
}
