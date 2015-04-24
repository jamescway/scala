import org.scalatest.FunSpec

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty)
      None
    else {
      val m = mean(xs)
      val sum = xs.foldright(0)( (x, acc) => math.pow(x - m,2) + acc )
      Some(sum / xs.length)
    }
  }


  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map(bb => f(aa,bb)) )
  }
  //above reduces to a flatMap(aa => Some(f(a)))
  //aa => Some(f(a)) matches the signature of what needs to be passed into flatmap
  //thus u get option c as the result of the flatmap

  // def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  //   a match{
  //     case None => Some(Nil)
  //     case h :: t => h flatmap( hh => List(x, sequence(t)) )

  //   }
  // }

  // Question: I don't understand how it gets flattened
  //   scala> List(1) :: List(2)
  //   res5: List[Any] = List(List(1), 2)
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))( (x, acc) => map2(x, acc)(_ :: _) )
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))( (x, acc) => map2(f(x), acc)(_ :: _))
  }

  def sequence_via_traverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a=>a)
  }

}


class OptionSpec extends FunSpec {

  describe("4.4 hasSubsequence") {
    it("should work"){
      assert(Option.sequence_2(List(Some(1), Some(2))) == Some(List(1,2)))
    }
  }
  describe("4.5 hasSubsequence") {
    it("traverse should work"){
      assert(Option.traverse(List(Some(1),Some(2))) ( (a) => a ) == Some(List(1,2)))
    }
    it("sequence via traverse should work"){
      assert(Option.sequence_via_traverse(List(Some(1), Some(2))( (a) => a )) == Some(List(1,2)))
    }
  }
}







