
// F is the first or front type, ie the type cons operand must match to be able to cons stuff to funclist
object Proof {
  case class MatchFront[A, F](implicitly: A =:= F)
  //def matchFront[A, B, F](implicit func: A => B, front: F) = { MatchFront[A, F] }
}

sealed trait HFuncList[F]{ type Front = F }
case class HFuncNil[A]() extends HFuncList[A] {
  override def toString = "FNil"
}
case class HFuncCons[A, B, +Next <: HFuncList[B]](f: A => B, next: Next)
  /* define implicit here that proves Next front type matches B type
  * we don't use it here but we can defer typechecking into the implicit
  * it is still a lot easier like this.
  * However this would enable more flexibility to the code
  * giving us the opportunity to inject other requirements */
  // (implicit funcMatchFrontType: Proof.MatchFront[B, Next#Front])
  extends HFuncList[A] {
  override def toString = s"FCons( ${f.toString}, ${next.toString} )"
}

object HFuncList {
  def fNil[A]: HFuncList[A] = HFuncNil[A]()
  def fCons[A, B](f: A => B, next: HFuncList[B]): HFuncList[A] =
    HFuncCons[A, B, HFuncList[B]] (f, next)
}


object example {
  import HFuncList._
  def id[A](a: A): A = a
  def incr(i: Int): Int = i + 1
  def intToString(i: Int): String = i.toString
  def stringToInt(s: String): Int = Integer.parseInt(s)

  // todo i'm obliged to enforce type on id function,
  // todo i'd rather like type inferencer to do this work :/
  val funcList =  fCons(stringToInt, fCons(id[Int], fCons(incr, fCons(intToString, fNil))))
}

example.funcList.toString


// todo implement map and fold over these structures