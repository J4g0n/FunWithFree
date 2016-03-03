package example

import fileInteraction.FileInteraction._
import interpreters._

object App {
  def main(args: Array[String]) = {
    val actions = for {
        _ <- open
        message <- write("Hello ")
        _ <- read
        _ <- read
        message2 <- write("world")
        _ <- read
        message2 <- write(" !")
        _ <- read
        _ <- close
    } yield ()


    stringInterpreter.interpret[Unit](actions)
    ioInterpreter.interpret[Unit]("C:\\Users\\ipi\\Desktop\\exercicesScala\\file.txt")(actions)
  }
}
