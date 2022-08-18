package snake

import org.jline.terminal.TerminalBuilder
import scala.concurrent._
import java.util.concurrent.Executors

object Main {
  var ch: Char = _

  def inputLoop: Unit = {
    val terminal = TerminalBuilder.builder()
      .jna(true)
      .system(true)
      .build()

    terminal.enterRawMode()

    val reader = terminal.reader()

    while (true) {
      ch = reader.read().toChar
      if (ch == 'q' || ch.toInt == 4) {
        reader.close()
        terminal.close()
        System.exit(0)
        return;
      }
    }
  }

  val directionStream = Stream.continually {
    Thread.sleep(1000)
    val result = ch match {
      case Key.Left => Direction.Left
      case Key.Right => Direction.Right
      case _ => Direction.Forward
    }
    ch = 0.toChar
    result
  }

  def main(args: Array[String]): Unit = {
    val game = new SnakeGame(6, 6)

    implicit val ec = {
      val executor = Executors.newFixedThreadPool(1)
      ExecutionContext.fromExecutor(executor)
    }

    // fire up the input reader
    Future { inputLoop }

    game.run(directionStream)
  }
}

object Key {
  val Up = 65
  val Left = 68
  val Right = 67
}
