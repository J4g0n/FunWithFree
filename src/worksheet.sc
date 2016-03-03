import cats.{Show, Functor}
import cats.free.Free


sealed trait CharToy[+Next]
object CharToy {
  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
      case o: CharOutput[A] => CharOutput(o.a, f(o.next))
      case b: CharBell[A]   => CharBell(f(b.next))
      case CharDone()       => CharDone()
    }
  }

  def output(a: Char): Free[CharToy, Unit] =
    Free.liftF[CharToy, Unit](CharOutput(a, ()))
  def bell: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharBell(()))
  def done: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharDone())
  def pure[A](a: A): Free[CharToy, A] = Free.pure[CharToy, A](a)
}

import CharToy._


implicit val showChar: Show[Char] = new Show[Char] {
  def show(char: Char) = char.toString
}
implicit val showUnit: Show[Unit] = new Show[Unit] {
  def show(char: Unit) = "()"
}

val subroutine = output('A')
val program = for {
  _ <- subroutine
  _ <- bell
  _ <- done
} yield ()

def showProgram[R: Show](p: Free[CharToy, R]): String =
  p.fold({ r: R => "return " + Show[R].show(r) + "\n" },
    {
      case CharToy.CharOutput(a, next) =>
        "output " + Show[Char].show(a) + " \n" + showProgram(next)
      case CharToy.CharBell(next) =>
        "bell " + "\n" + showProgram(next)
      case CharToy.CharDone() =>
        "done\n"
    })

showProgram(program)
showProgram(output('A'))
showProgram(pure('A') flatMap output)
showProgram(output('A') flatMap pure)
showProgram((output('A') flatMap { _ => done }) flatMap { _ => output('C') })
showProgram(output('A') flatMap { _ => (done flatMap { _ => output('C') }) })