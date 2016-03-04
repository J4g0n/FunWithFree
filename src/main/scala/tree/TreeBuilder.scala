package tree

import cats.Functor

/**
  * Created by ipi on 04/03/2016.
  */
// This is a tree of values at nodes and function that has cont
sealed abstract trait TreeBuilder[Next]
case class NodeBuilder[F[_], A](cont: F[A], treeBuilders: List[TreeBuilder]) extends TreeBuilder[A]
case class LeafBuilder[A](value: A) extends TreeBuilder[A]

object TreeBuilder {
  type RightFunc[A] = ({
    type lambda[x] = x => A
  })#lambda
  case class BuiltNode[A](override val cont: RightFunc[A], override val treeBuilders: List[TreeBuilder])
    extends NodeBuilder[RightFunc[A], A](cont, treeBuilders)
  case class BuiltLeaf[A](override val value: A) extends LeafBuilder[A](value)

  type Node[A] = NodeBuilder[({ type lambda[x] = x => A })#lambda, A]

  implicit val treeBuilderFunctor = new Functor[TreeBuilder] {
    def map[A, B](fa: TreeBuilder[A])(f: A => B): TreeBuilder[B] = {
      fa match {
        case BuiltNode(cont, Nil) => compose(cont, f)
        case NodeBuilder(cont, builtList) => cont()
        case LeafBuilder(value) =>
      }
    }
  }
}
