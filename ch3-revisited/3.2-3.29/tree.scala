import org.scalatest.FunSpec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]) : Int =  t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maxi(t: Tree[Int]) : Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maxi(left) max maxi(right)
  }

  def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold(t: Tree[A])(f1: A => B)(f2: A,B => B) : Tree[B] = {
    case Leaf(value) => f1(value)
    case Branch(left, right) => foldright( f(left, right)
  }
}

class ListSpec extends FunSpec {
  val tree2    = new Branch(new Leaf(4), new Leaf(5))
  val tree2_str= new Branch(new Leaf("4"), new Leaf("5"))
  val tree3    = new Branch(tree2, tree2)
  val tree_max = new Branch(new Branch(new Leaf(4), new Leaf(5)), new Branch(new Leaf(6), new Leaf(7)))
  val tree4    = new Branch(tree3, tree3)

  describe("3.25 size") {
    it("should work with height 2"){
      assert(Tree.size(tree2) == 3)
    }
    it("should work with height3"){
      assert(Tree.size(tree3) == 7)
    }
    it("should work with height4"){
      assert(Tree.size(tree4) == 15)
    }
  }
  describe("3.26 max") {
    it("should work"){
      assert(Tree.maxi(tree_max) == 7)
    }
  }
  describe("3.27 depth") {
    it("should work"){
      assert(Tree.depth(tree4) == 4)
    }
  }
  describe("3.28 map") {
    it("should work"){
      assert(Tree.map(tree2)(a => a.toString) == tree2_str)
    }
  }
}







