package snake

import org.jline.terminal.TerminalBuilder
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executors

object Main {
  val game = new SnakeGame(6, 6)

  var ch: Char = _

  def done = game.isGameOver

  def inputLoop: Unit = {
    val terminal = TerminalBuilder.builder()
      .jna(true)
      .system(true)
      .build()

    terminal.enterRawMode()

    val reader = terminal.reader()

    do {
      ch = reader.read().toChar
      if (ch == 'q' || ch.toInt == 4) {
        reader.close()
        terminal.close()
        return;
      }
    } while (!done)
  }

  val stepTime = 500.millis

  val directionStream = Stream.continually {
    Thread.sleep(stepTime.toMillis)

    val direction = game.state.snake.direction
    // the following could be simplified
    // but this was the most straight forward way
    // to map the perspective of the player
    // to the snakes perspective
    val result = (direction, ch) match {
      case (Pos.Up, Key.Left) =>
        Direction.Left
      case (Pos.Up, Key.Right) =>
        Direction.Right
      case (Pos.Down, Key.Left) =>
        Direction.Right
      case (Pos.Down, Key.Right) =>
        Direction.Left
      case (Pos.Left, Key.Up) =>
        Direction.Right
      case (Pos.Left, Key.Down) =>
        Direction.Left
      case (Pos.Right, Key.Up) =>
        Direction.Left
      case (Pos.Right, Key.Down) =>
        Direction.Right
      case (_, _) =>
        Direction.Forward
    }
    ch = 0.toChar // reset
    result
  }.takeWhile(_ => !done)

  def main(args: Array[String]): Unit = {

    implicit val ec = {
      val executor = Executors.newFixedThreadPool(1)
      ExecutionContext.fromExecutor(executor)
    }

    // fire up the input reader
    Future { inputLoop }

    // initial draw of the field
    println(game.fieldMap)

    directionStream.foreach { direction =>
      game.run(direction) match {
        case None =>
          println(s"Game over")
        case Some(state) =>
          println(EscapeCode.right(11) + EscapeCode.up(7))
          println(game.fieldMap)
      }
    }
  }
}

object Key {
  val Up = 65
  val Left = 68
  val Right = 67
  val Down = 66
}

object EscapeCode {
  def right(n: Int) = s"\u001b[${n}C"
  def up(n: Int) = s"\u001b[${n}A"
}