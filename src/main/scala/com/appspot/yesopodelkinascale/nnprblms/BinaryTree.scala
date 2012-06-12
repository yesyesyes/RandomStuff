package scalaproblems

sealed abstract class Tree[+T] {
  def isSymmetric: Boolean
  def isMirrorOf[V](t: Tree[V]): Boolean
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def isSymmetric = left isMirrorOf right 
  override def isMirrorOf[V](t: Tree[V]) = t match {
    case t: Node[V] => (left isMirrorOf t.left) && (right isMirrorOf t.right)
    case _ => false
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."
  override def isSymmetric = true
  override def isMirrorOf[V](t: Tree[V]) = t == End
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

/**
 * P55 (**) Construct completely balanced binary trees.
 * In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
 *
 * Define an object named Tree. Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions. The function should take as parameters the number of nodes and a single value to put in all of them.
 *
 * scala> Tree.cBalanced(4, "x")
 * res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
 */
object P55 {
  def main(args: Array[String]) {
    println(cBalanced(5, "x"))
  }

  def cBalanced[A](n: Int, x: A): List[Tree[A]] =
    n match {
      case 0 => List(End)
      case n if n % 2 == 1 =>
        val left = cBalanced(n / 2, x)
        left.flatMap(l => left.map(r => Node(x, l, r)))
      case n if n % 2 == 0 =>
        val lesser = cBalanced((n - 1) / 2, x)
        val greater = cBalanced((n + 1) / 2, x)
        cBalanced((n - 1) / 2, x) flatMap { l => greater flatMap { r => List(Node(x, l, r), Node(x, r, l)) } }
    }

}

/**
 * P56 (**) Symmetric binary trees.
 * Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
 *
 * scala> Node('a', Node('b'), Node('c')).isSymmetric
 * res0: Boolean = true
 */
object P56 {
  def main(args: Array[String]) {
    println(Node('a', Node('b'), Node('c')).isSymmetric)
  }

case class TreeSymmetryTest[A](t: Tree[A]) {
    def isSymmetric = t match {
      case End => true
      case Node(_, l, r) => isMirror(l, r)
    }
    def isMirror[A](l: Tree[A], r: Tree[A]): Boolean = (l, r) match {
    case (Node(_, l: Node[A], r: Node[A]), Node(_, l1: Node[A], r1: Node[A])) => 
      isMirror(l, l1) && isMirror(r, r1)
    case (Node(_, End, r: Node[A]), Node(_, End, r1: Node[A])) => isMirror(r, r1)
    case (Node(_, l: Node[A], End), Node(_, l1: Node[A], End)) => isMirror(l, l1)
    case (Node(_, End, End), Node(_, End, End)) => true
    case (_, _) => false
    }
  }
}













