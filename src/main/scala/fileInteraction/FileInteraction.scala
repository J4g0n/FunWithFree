package fileInteraction

import cats.Functor
import cats.free.Free


/**
  * Created by ipi on 03/03/2016.
  */

trait FileInteraction[+A]
object FileInteraction {
  // Step 1: declare domain with actions
  case class Error(errorMessage: String) extends FileInteraction[Nothing]
  case class OpenFile[A](nextAction: A) extends FileInteraction[A]
  case class CloseFile[A](nextAction: A) extends FileInteraction[A]
  case class ReadFile[A](nextAction: A) extends FileInteraction[A]
  case class WriteFile[A](writeString: String, nextAction: A) extends FileInteraction[A]

  // Step 2: declare functor that maps over these actions
  implicit def fileInteractionFunctor: Functor[FileInteraction] = new Functor[FileInteraction] {
    def map[A, B](fa: FileInteraction[A])(f: A => B): FileInteraction[B] = {
      fa match {
        case ReadFile(nextAction) => ReadFile(f(nextAction))
        case WriteFile(writeString, nextAction) => WriteFile(writeString, f(nextAction))
        case OpenFile(nextAction) => OpenFile(f(nextAction))
        case CloseFile(nextAction) => CloseFile(f(nextAction))
        case Error(errorMessage) => Error(errorMessage)
      }
    }
  }

  // Step 4(optional): declare type alias for free on these actions
  type FileAction[A] = Free[FileInteraction, A]

  def open: FileAction[Unit] = Free.liftF(OpenFile())
  def close: FileAction[Unit] = Free.liftF(CloseFile())
  def read: FileAction[Unit] = Free.liftF(ReadFile())
  def write(writeString: String): FileAction[String] = Free.liftF(WriteFile(writeString, writeString))
  def error(errorMessage: String): FileAction[Nothing] = Free.liftF(Error(errorMessage))
}