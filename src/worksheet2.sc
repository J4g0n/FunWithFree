
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



// Recursivity in scala's types
trait Recurse {
  type Next <: Recurse
  type X[R <: Recurse] <: Int
}

trait RecurseA extends Recurse {
  type Next = RecurseA
  type X[R <: Recurse] = R#X[R#Next]
}

/*object Recurse {
// infinite loop example: it seems like compiler knows how to detect it
  type C = RecurseA#X[RecurseA]
}*/


// Type level booleans
sealed trait Bool {
  type If[T <: Up, F <: Up, Up] <: Up
}
trait True extends Bool {
  type If[T <: Up, F <: Up, Up] = T
}
trait False extends Bool {
  type If[T <: Up, F <: Up, Up] = F
}

object Bool {
  type Not[A <: Bool] = A#If[False, True, Bool]
  type &&[A <: Bool, B <: Bool] = A#If[B#If[B, B, Bool], A, Bool]
  type ||[A <: Bool, B <: Bool] = A#If[A, B#If[B, B, Bool], Bool]
  // <=> type ||[A <: Bool, B <: Bool] = Not[&&[Not[A], Not[B]]]
}

import Bool._

type Rep[B <: Bool] = B#If[Int, String, Any]
type OrToAnd[A <: Bool, B <: Bool] = Not[&&[Not[A], Not[B]]]

implicitly[Rep[True] =:= Int]

implicitly[True || False =:= OrToAnd[True, False]]
implicitly[True || True =:= OrToAnd[True, False]]
implicitly[False || False =:= Not[OrToAnd[True, False]]]


// Naturals
sealed trait Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up

  type gt = Match[False, False, True, Bool]
  type ge = Match[False, True, True, Bool]
  type lt = Match[True, True, False, Bool]
  type le = Match[True, False, False, Bool]
  type eq = Match[False, True, False, Bool]
}
sealed trait GT extends Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
}
sealed trait LT extends Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
}
sealed trait EQ extends Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
}


trait Fold[-Elem, Value] {
  type Apply[E <: Elem, V <: Value] <: Value
}


sealed trait Nat {
  type Match[NonZero[M <: Nat] <: Up, IfZero <: Up, Up] <: Up

  type Compare[M <: Nat] <: Comparison

  type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] <: Type
}
sealed trait _0 extends Nat {
  type Match[NonZero[M <: Nat] <: Up, IfZero <: Up, Up] = IfZero

  type ConstLT[A] = LT
  type Compare[M <: Nat] = M#Match[ConstLT, EQ, Comparison]

  type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] = Init
}
sealed trait Succ[N <: Nat] extends Nat {
  type Match[NonZero[M <: Nat] <: Up, IfZero <: Up, Up] = NonZero[N]

  type Compare[M <: Nat] = M#Match[N#Compare, GT, Comparison]

  type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] =
    F#Apply[Succ[N], N#FoldR[Init, Type, F]]
}


object Nat {
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]

  type Incr = Fold[Nat, Nat] {
    type Apply[A <: Nat, B <: Nat] = Succ[B]
  }
  type +[N <: Nat, M <: Nat] = N#FoldR[M, Nat, Incr]

  // Ghost type (like Const but for 2-arity type constructor)
  type Sum[By <: Nat] = Fold[Nat, Nat] {
    type Apply[A <: Nat, Acc <: Nat] = By + Acc
  }
  type Mult[N <: Nat, M <: Nat] = N#FoldR[_0, Nat, Sum[M]]

  // todo implement factorial
  type Prod = Fold[Nat, Nat] {
    type Apply[A <: Nat, Acc <: Nat] = A Mult Acc
  }
  type Fact[N <: Nat] = N#FoldR[_1, Nat, Prod]

  type ProdExp[By <: Nat] = Fold[Nat, Nat] {
    type Apply[A <: Nat, B <: Nat] = By Mult B
  }
  type Exp[N <: Nat, E <: Nat] = E#FoldR[_1, Nat, ProdExp[N]]

  type ModFold[By <: Nat] = Fold[Nat, Nat] {
    type Wrap[C <: Nat] = By#Compare[C]#eq
    type Apply[A <: Nat, Acc <: Nat] = Wrap[Succ[Acc]]#If[_0, Succ[Acc], Nat]
  }
  type Mod[N <: Nat, M <: Nat] = N#FoldR[_0, Nat, ModFold[M]]
}


import Nat._

type ConstFalse[A] = False
type Is0[N <: Nat] = N#Match[ConstFalse, True, Bool]

implicitly[_3 + _4 =:= _7]
implicitly[_3 Mult _2 =:= _6]
implicitly[(_3 + _1) Mult _2 =:= _8]
// implicitly[_3 Mod _2 =:= _1] todo make this work, i don't why it is not provable


// Digit
sealed trait Digit {
  type Match[IfOne <: Up, IfZero <: Up, Up] <: Up
  type Compare[N <: Digit] <: Comparison
}
sealed trait Zero extends Digit {
  type Match[IfOne <: Up, IfZero <: Up, Up] = IfZero
  type Compare[N <: Digit] = N#Match[GT, EQ, Comparison]
}
sealed trait One extends Digit {
  type Match[IfOne <: Up, IfZero <: Up, Up] = IfOne
  type Compare[N <: Digit] = N#Match[EQ, LT, Comparison]
}

sealed trait Dense {
  type digit <: Digit
  type tail <: Dense
  type Inc <: Dense
  type ShiftR <: Dense
  type ShiftL <: Dense
}
sealed trait DCons[d <: Digit, T <: Dense] extends Dense {
  type digit = d
  type tail = T
  type Inc = d#Match[Zero :: T#Inc, One :: T, Dense]
  type ShiftR = tail
  type ShiftL = Zero :: DCons[d, T]
}
sealed trait DNil extends Dense {
  type digit = Nothing
  type tail = Nothing
  type Inc = One :: DNil
  type ShiftR = DNil
  type ShiftL = DNil
}
type ::[d <: Digit, T <: Dense] = DCons[d, T]

object Dense {
  type _0 = Zero :: DNil
  type _1 = One :: DNil
  type _2 = One :: Zero :: DNil
  type _3 = One :: One :: DNil
  type _4 = One :: Zero :: Zero :: DNil
  type _5 = One :: Zero :: One :: DNil
}



// HList implementation
// todo does not work try: https://github.com/harrah/up/blob/master/HList.scala
sealed trait HList {
  type Foldr[Value, F <: Fold[Any, Value], I <: Value] <: Value
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: Value): Foldr[Value, F, I]

  type Foldl[Value, F <: Fold[Any, Value], I <: Value] <: Value
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: Value): Foldr[Value, F, I]
}

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  def ::[V](obj: V) = HCons(obj, this)

  type Foldr[Value, F <: Fold[Any, Value], I <: Value] =
    F#Apply[H, tail.Foldr[Value, F, I]]
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: Value): Foldr[Value, F, I] =
    f(head, tail.foldr[Value, F, I](f, i))

  type Foldl[Value, F <: Fold[Any, Value], I <: Value] =
    tail.Foldl[Value, F, F#Apply[H, I]]
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: Value): Foldr[Value, F, I] =
    tail.foldl[Value, F, F#Apply[H, I]](f, f(head, i))
}

sealed class HNil extends HList {
  def ::[V](obj: V) = HCons(obj,this)

  type Foldr[Value, F <: Fold[Any, Value], I <: Value] = I
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: Value): Value = i

  type Foldl[Value, F <: Fold[Any, Value], I <: Value] = I
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: Value): Value = i
}
case object HNil extends HNil

object HList {
  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
}

import HList._
val x = "hello" :: true :: 13 :: HNil

val str: String = x.head
val bool: Boolean = x.tail.head
val i: Int = x.tail.tail.head
val error: HNil = x.tail.tail.tail