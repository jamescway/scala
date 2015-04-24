import org.scalatest.FunSpec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], new_head: A): List[A] = {
    l match {
      case Nil => List(new_head)
      case Cons(h, t) => Cons(new_head, Cons(h, t))
    }
  }

  def drop[A](l: List[A], i: Int): List[A] = {
    if(i == 0)
      return l

    l match {
      case Nil => l
      case Cons(h, t) => drop(t, i - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => l
      case Cons(h, t) => if(f(h) == true) dropWhile(t, f) else t
    }
  }


  // 1,2,3,nil
  // h=1 t=2,3,nil   return [1]h + init(2,3,nil)
  // h=2 t=3,nil     return [2]h + init(3,nil)
  // h=3 t=nil       return nil

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(h, t) => if(t == Nil) Nil else return Cons(h, init(t))
    }
  }

  // 3.7 cannot short circut because its not tail recursive

  //3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => 1 + y)
  }

  //3.10
  // foldright(1,2,3,N  0)
  // 1 + foldright(2,3,N  0)
  // 1 + (2 + foldright(3,N  0) )
  // 1 + (2 + (3 + (foldright(N) 0)
  // 1 + (2 + (3 + (0)))

  // foldleft(1,2,3,N  0)(y,x => x + y)
  // foldleft(2,3,N  1) (0 + 1)
  // foldleft(3,N  3) (1 + 2)
  // foldleft(N  6) (3 + 3)
  // foldleft(   6) (0 + 6)
  // 6

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, y) => foldLeft(y, f(z, x))(f)
    }

  }

  // 3.11
  def sumLeft(l: List[Int]) = {
    foldLeft(l, 0)((x, y) => x + y)
  }
  def productLeft(l: List[Int]) = {
    foldLeft(l, 1)((x, y) => x * y)
  }
  def lengthLeft[A](l: List[A]) : Int = {
    //x is the accumulator (i.e. 'z')
    foldLeft(l, 0)((x, y) => x + 1)
  }

  //3.12
  // fl(1,2,3,N  N)
  // fl(2,3,N  N)   2,3,N, Cons(N, 1)
  // fl(3,N  Cons(N,1))  3,N, Cons(N,1,2)
  // fl(N, Cons(N,1,2))  N, Cons(N,1,2,3)
  def reverse[A](l: List[A]) : List[A] = {
    foldLeft(l, List[A]())( (x, y) => Cons(y, x) )
  }

  //
  def foldleft_using_foldright[A, B](l: List[A], z: B)(f: (A, B) => B) : B = {
    foldRight(reverse(l), z)((a, b) => f(a, b))
  }

  // to simulate fold right with fold left
  //  1. you have to reverse the input list
  //  2. Then reverse the input parameters because it gets reversed too
  // def foldright_using_foldleft[A, B](l: List[A], z: B)(f: (A, B) => B) : B = {
    // foldLeft(reverse(l), z)((b, a) => f(a, b))
  // }
  def append_using_foldRight[A](l1: List[A], l2: List[A]) : List[A] = {
    foldRight(l1, l2)((a, b) => Cons(a, b))
  }

  def concat[A](lofl: List[List[A]]) : List[A] = {
    foldRight(lofl, List[A]() )( (a,b) => append(a, b) )
  }

  def addone(l: List[Int]) : List[Int] = {
    foldRight(l, List[Int]())( (a, b) => Cons(a + 1,b) )
  }

  def stringify(l: List[Double]) : List[String] = {
    foldRight(l, List[String]())( (a, b) => Cons(a.toString, b))
  }

  def map[A,B](l: List[A])(f: A => B) : List[B] = {
    foldRight(l, List[B]())((a, z) => Cons(f(a), z))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())( (a, b) => if(f(a)) Cons(a, b) else b )
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)( (a) => if(f(a)) List(a) else Nil )
  }

  def addPairwiseMine(l1: List[Int], l2: List[Int]): List[Int] = {
    // Cons(l1.head + l2.head, zip(l1.tail, l2.tail))
    val x = l1 match {
      case Nil => -1
      case Cons(h,t) => h
    }
    val y = l2 match {
      case Nil => -1
      case Cons(h,t) => h
    }
    if (x != -1 && y != -1) Cons(x + y, addPairwiseMine(tail(l1), tail(l2))) else Nil
  }
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  def zipWith[A,B,C](l1: List[A], l2:List[B])(f: (A, B) => C) : List[C] = (l1,l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def startsWith[A](sup: List[A],sub: List[A]) : Boolean = (sup, sub) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(h1,t1), Cons(h2,t2)) => if(h1 != h2) false else startsWith(t1, t2)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if(startsWith(sup, sub)) true else startsWith(tail(sup), sub)
  }

}

class ListSpec extends FunSpec {
  describe("#tail") {
    it("should work here"){
      assert(List.tail[Int](List(1,2,3,4)) == List(2,3,4))
    }
    it("should work with empty"){
      assert(List.tail[Int](List()) == List())
    }
  }
  describe("#setHead") {
    it("should work"){
      assert(List.setHead[Int](List(1, 2), 0) == List(0, 1, 2))
    }
    it("should work with nil list"){
      assert(List.setHead[Int](Nil, 0) == List(0))
    }
    // it("should work with nil head"){
    //Type error here on compile but what if in runtime it gets a Nil
    //assert(List.setHead[Int](List(1, 2), Nil) == List(1, 2))
    // }
  }
  describe("#drop") {
    it("should work"){
      assert(List.drop[Int](List(1,2,3,4), 2) == List(3,4))
    }
    it("should work with nil"){
      assert(List.drop[Int](Nil, 2) == Nil)
    }
  }
  describe("#dropWhile") {
    it("should work"){
      assert(List.dropWhile[Int](List(1,2,3,4), (a: Int) => { a < 2 }) == List(3,4))
    }
    it("should work with nil list"){
      assert(List.dropWhile[Int](Nil, (a: Int) => { a < 2 }) == Nil)
    }
    it("should work with empty list"){
      assert(List.dropWhile[Int](List(), (a: Int) => { a < 2 }) == List())
    }
    it("should work with different comparison"){
      assert(List.dropWhile[Int](List(1,2,3,4), (a: Int) => { a != 2 }) == List(3,4))
    }
  }
  describe("#init") {
    it("should work"){
      assert(List.init[String](List("1","2","3","4")) == List("1","2","3"))
    }
  }
  describe("3.8"){
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  }
  describe("3.9 it should work"){
    assert(List.length(List(1,2,3)) == 3)
    assert(List.length(List()) == 0)
  }
  describe("3.11 sum product length") {
    it("sum should work") {
      assert(List.sumLeft(List(1,2,3)) == 6)
    }
    it("product should work") {
      assert(List.productLeft(List(1,2,3)) == 6)
    }
    it("length should work") {
      assert(List.lengthLeft(List(1,2,3)) == 3)
      assert(List.lengthLeft(List("a","b","c","d")) == 4)
    }
  }
  describe("3.12 reverse") {
    it("should work") {
      assert(List.reverse(List(1,2,3)) == List(3,2,1))
    }
  }
  describe("3.13"){
    it("uses fold left in terms of fold right"){

    }
  }
  describe("3.14 append using fold"){
    it("should work"){
      assert(List.append_using_foldLeft(List(1,2), List(3,4)) == List(1,2,3,4))
    }
  }
  describe("3.15 concat"){
    it("should work"){
      assert(List.concat(List(List(3,4),List(5,6))) == List(3,4,5,6))
    }
  }
  describe("3.16 plus one to each") {
    it("should work"){
      assert(List.addone(List(3,4)) == List(4,5))
    }
  }
  describe("3.17 stringify doubles") {
    it("should work"){
      assert(List.stringify(List(3.0,4.0)) == List("3.0","4.0"))
    }
  }
  describe("3.18 map") {
    it("should work with integer sums"){
      assert(List.map(List(3,4))(a => a + 1) == List(4,5))
    }
    it("should work with stringify"){
      assert(List.map(List(3,4))(a => a.toString) == List("3","4"))
    }
  }
  describe("3.19 filter") {
    it("should work"){
      assert(List.filter(List(3,4,5))(a => a != 4) == List(3,5))
    }
  }
  describe("3.20 flatmap") {
    it("should work"){
      assert(List.flatMap(List(1,2,3))(a => List(a,a)) == List(1,1,2,2,3,3))
    }
  }
  describe("3.21 filter") {
    it("should work"){
      assert(List.filterFlatMap(List(3,4,5))(a => a != 4) == List(3,5))
    }
  }
  describe("3.22 zip") {
    it("should work"){
      assert(List.addPairwise(List(3,4,5), List(1,1,1)) == List(4,5,6))
    }
  }
  describe("3.23 zip") {
    it("should work"){
      assert(List.zipWith(List(3,4,5), List(1,1,1))((a,b)=> a + b) == List(4,5,6))
    }
  }
  describe("3.24 hasSubsequence") {
    it("should work"){
      assert(List.hasSubsequence(List(2,3,4,5), List(3,4)) == true)
    }
    it("should not work"){
      assert(List.hasSubsequence(List(2,3,4,5), List(9,4)) == false)
    }
  }




}







