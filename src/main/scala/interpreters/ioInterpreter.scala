package interpreters

import java.io.{FileWriter, PrintWriter, File}

import fileInteraction.FileInteraction._

import scala.io.Source

/**
  * Created by ipi on 03/03/2016.
  */
object ioInterpreter {
  def interpret[A](filePath: String)(actions: FileAction[A]): Unit = {
    actions.fold(
      { v => println("done\n") },
      {
        case OpenFile(next) =>
          val file: File = new File(filePath)
          if (file.exists()) {
            interpret(file)(next)
          }
        case _ => println("error\n")
      })
  }

  private def interpret[A](file: File)(actions: FileAction[A]): Unit = actions.fold(
    { v => println("done\n") },
    {
      case OpenFile(next) =>
        println("open")
        println("Warning: file already opened")
        interpret(file)(next)
      case WriteFile(msg, next) =>
        println(s"write `$msg`")
        val out = new FileWriter(file, true)
        out.write(msg)
        out.close()
        interpret(file)(next)
      case ReadFile(next) =>
        println("read")
        for (line <- Source.fromFile(file.getAbsolutePath).getLines()) {
          println("\t" + line)
        }
        interpret(file)(next)
      case Error(msg) => println("Error: $msg")
      case CloseFile(next) => println("close")
    })
}
