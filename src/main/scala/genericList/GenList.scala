package genericList

import cats.Functor
import cats.data.Func


// code borrowed from:
// http://eed3si9n.com/herding-cats/datatype-generic-programming.html
// see also usefull resources:
// https://dzone.com/articles/datatype-generic-programming
sealed abstract class Fix[S[_], A] extends Serializable {
  def out: S[Fix[S, A]]
}
object Fix {
  case class In[S[_], A] (out: S[Fix[S, A]]) extends Fix[S, A]
}

sealed trait ListF[+Next, +F]
object ListF{
  case object NilF extends ListF[Nothing, Nothing]
  case class ConsF[F, Next](a: F, next: Next) extends ListF[Next, F]
}
object GenericList {
  def nil[A]: Fix[ListF[+?, A], A] = Fix.In[ListF[+?, A], A] (ListF.NilF)
  def cons[A](a: A, xs: Fix[ListF[+?, A], A]): Fix[ListF[+?, A], A] =
    Fix.In[ListF[+?, A], A] (ListF.ConsF(a,xs))
}


/*sealed trait FuncF[+Next, -F]
object FuncF {
  case object Id extends FuncF[Nothing, Nothing]
  case class Expr[T, Next](t: T, rest: Next) extends FuncF[Next, T]
}
object GenericFunc {
  def id[A]: Fix[FuncF[+?, A], A] = Fix.In[FuncF[+?, A], A] (FuncF.Id[A])
  def expr[A](t: A, rest: Fix[FuncF[+?, A], A]): Fix[FuncF[+?, A], A] =
    Fix.In[FuncF[+?, A], A] (FuncF.Expr(t, rest))
}*/


object FunctionOps {
  // easy on this one codomain type is covariant
  // use Contravariant functor when used on domain type
  // or use profunctor and declare both once and for all
  // we could have used this without type lambdas see:
  // https://meta.plasm.us/posts/2015/07/11/roll-your-own-scala/
  implicit def funcFunctor[T]: Functor[Lambda[X => Function1[T, X]]] = {
    new Functor[Lambda[X => Function1[T, X]]] {
      def map[A, B](fa: T => A)(f: A => B): T => B = fa andThen f
    }
  }
}


//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////
// todo maybe try to use implicit and add some functions to Function1
sealed trait UnaryFunc[-S, +T]
object UnaryFunc {
  case object NilFunc extends UnaryFunc[Nothing, Nothing]
  case class BottomFunc[A](a: A) extends UnaryFunc[A, Nothing]
  case class Id[A, B](dom: A) extends UnaryFunc[A, A]
  case class Func[A, B](dom: A, codom: B) extends UnaryFunc[A, Nothing]
}

// here we use next that should be FuncList itself we are going to try to try to use it with Fix point defined above
sealed trait FuncListF[+Next, Function1[-S, +T]]
object FuncListF {
  case object NilFuncList extends FuncListF[Nothing, Nothing]
  case class PrependFunc[A, B, Next](f: A => B, next: Next) extends FuncListF[Next, A => B]
}
object GenericFunc {
  def prependFunc[A, B](f: A => B, next: Fix[FuncListF[+?, B => _], B => _])
    : Fix[FuncListF[+?, A => B], A => B] =
      Fix.In[FuncListF[+?, A => B], A => B] (FuncListF.PrependFunc(f, next))
  def nil[A]: Fix[FuncListF[A, ?], A] = Fix.In[FuncListF[A, ?], A] (FuncListF.NilFuncList[A])
}


