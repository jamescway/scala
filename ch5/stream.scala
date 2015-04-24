import org.scalatest.FunSpec
import Stream._

trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }
  def take_2(n: Int) : Stream[A] = {
    def go(s: Stream[A], n: Int) : Stream[A] = {
      s match {
        case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
        case Cons(h, t) => Stream.empty
      }
    }
    go(this, n)
  }
  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h,t) if n > 0 => t().drop(n - 1)
      case Cons(h,t) => this
    }
  }
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def forAll(p: A => Boolean): Boolean = {
    foldRight(false)((a,b) => p(a) && b)
  }
  def takeWhile_foldr(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,acc) => if(p(a)) cons(a, acc) else empty)
  }
  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, acc) => cons(f(a),acc))
  }
  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, acc) => if(f(a)) cons(a, acc) else acc)
  }
  // ERROR found   : a.type (with underlying type A)
  //    fixed: output stream should have been Stream[B] not Stream[A]
  //           then added the B>:A in the front because they are the somewhat the same type
  def append_2[B >: A](b: => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a,acc) => if(acc != empty) cons(a, acc) else cons(a, b) )
  }
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, acc) => f(a).append(acc) )
  }

  def map_unfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }
  }

  def take_unfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some( (h(), (t(),n-1)) )
      case _ => None
    }
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B,C](s2:Stream[B])(f: (A,B) => C) : Stream[C] = {
    unfold(this, s2) {
      case(Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {

  }

  def startsWith[A](s: Stream[A]): Boolean = {

  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // def constant_2[A](a: A): Stream[A] = {
  //   val constants = Stream.cons(a, constant_2(a))
  // }
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons( () => a, () => tail )
    tail
  }
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }
  def fibs: Stream[Int] = {
    // 0, 1, 1, 2, 3, 5, 8
    def go(n: Int, m: Int): Stream[Int] = {
      cons(n, go(m, n + m))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  // 0,1,1,2,3,5
  def fibs_unfold =
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }


  def from_unfold(n: Int): Stream[Int] = {
    unfold(n)(a => Some(a, a+1))
  }

  def constant_unfold[A](a: A): Stream[A] = {
    unfold(a)(a => Some(a, a))
  }
  def ones_unfold: Stream[Int] = {
    unfold(1)(a => Some(a, a))
  }
}

class StreamSpec extends FunSpec {
  describe("5.1 to list") {
    it("should work"){
      assert(Stream(1,2).toList == List(1,2))
    }
  }
  describe("5.2") {
    it("take should work"){
      assert(Stream(1,2,3).take(1).toList == List(1))
    }
    it("drop should work"){
      assert(Stream(1,2,3).drop(1).toList == List(2,3))
    }
  }
  describe("5.3") {
    it("takeWhile should work"){
      assert(Stream(1,2,3).takeWhile(a => a != 2).toList == List(1))
    }
  }
  describe("5.5") {
    it("takeWhile with foldr should work"){
      assert(Stream(1,2,3).takeWhile_foldr(a => a != 2).toList == List(1))
    }
  }
  describe("5.6") {
    it("headoption should work"){
      assert(Stream(1,2,3).headOption == Option(1))
      assert(Stream().headOption == None)
    }
  }
  describe("5.7") {
    it("map should work"){
      assert(Stream(1,2).map(_ + 5).toList == List(6,7))
    }
    it("filter should work"){
      assert(Stream(1,2,3).filter(a => a == 2).toList == List(2))
    }
    it("append should work"){
      assert(Stream(1,2).append(Stream(3,4)).toList == List(1,2,3,4))
    }
    it("flatmap should work"){
      assert(Stream(1,2).flatMap(a => Stream(a,a)).toList == List(1,1,2,2))
    }
  }
  describe("5.8") {
    it("constant should work"){
      assert(Stream.constant(2).take(3).toList == List(2,2,2))
    }
  }
  describe("5.9") {
    it("from should work"){
      assert(Stream.from(2).take(3).toList == List(2,3,4))
    }
  }
  describe("5.10") {
    it("fibs should work"){
      assert(Stream.fibs.take(5).toList == List(0,1,1,2,3))
    }
  }
  describe("5.11") {
    it("unfold should work"){
      assert(Stream.unfold(1)(a => Some((a, a+1))).take(3).toList == List(1,2,3))
    }
  }
  describe("5.12") {
    it("fibs_unfold should work"){
      // assert(Stream.fibs_unfold.take(5).toList == List(0,1,1,2,3))
    }
    it("from_unfold should work"){
      assert(Stream.from_unfold(2).take(3).toList == List(2,3,4))
    }
    it("constant_unfold should work"){
      assert(Stream.constant_unfold(2).take(3).toList == List(2,2,2))
    }
    it("ones_unfold should work"){
      assert(Stream.ones_unfold.take(3).toList == List(1,1,1))
    }
  }
  describe("5.13") {
    it("map_unfold should work"){
      assert(Stream(1,2,3).map_unfold(a => a+1).take(3).toList == List(2,3,4))
    }
    it("take_unfold should work"){
      assert(Stream(1,2,3).take_unfold(2).toList == List(1,2))
    }
    it("takeWhile_unfold should work"){
      assert(Stream(1,2,3).takeWhile_unfold(a => a != 2).toList == List(1))
    }
    it("zipWith should work"){
      assert(Stream(1,2,3).zipWith(Stream(4,4,4))((a,b) => a+b).toList == List(5,6,7))
    }
    it("zipAll should work"){
      // assert(Stream(1,2,3).zipWith(Stream(4,4,4))((a,b) => a+b).toList == List(5,6,7))
    }
  }
  describe("5.14") {
    it("startsWith should work"){
      assert(Stream(1,2,3,4).startsWith(Stream(1,2)) == true)
    }
  }

}
