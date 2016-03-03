package interpreters

import fileInteraction.FileInteraction._

/**
  * Created by ipi on 03/03/2016.
  */
object stringInterpreter {
  def interpret[A](actions: FileAction[A]): Unit = actions.fold(
    { v => println("done\n") },
    {
      case OpenFile(next) => println("open"); interpret(next)
      case CloseFile(next) => println("close"); interpret(next)
      case WriteFile(msg, next) => println(s"write $msg"); interpret(next)
      case ReadFile(next) => println("read"); interpret(next)
    })
}
