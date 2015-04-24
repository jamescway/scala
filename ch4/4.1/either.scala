import org.scalatest.FunSpec
import scala.{Option => _, Either => _, _}

sealed trait Either[+E,+A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap(EE >: E, B)(f: A => Either[EE,B]): Either[EE,B] = this match {
    case Right(a) => f(a)
    case Left(e) => e
  }
  def orElse(EE >: E, B >: A)(b: Either[EE,B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  // I don't understand how the return gets assigned to Either[EE, C]
  // ANSWER: because the flatmap returns an Either
  def map2[EE >: E,B,C](b: Either[EE,B])(f: (A, B) => C): Either[EE, C] = {
    Either[EE, C] = for {
      tt <- t
      bb <- b
    } yield f(tt, bb)
  }


}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight[Either[E, List[B]](Left(Nil))( (x, acc) => f(x).map2(acc)(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }


}


class EitherSpec extends FunSpec {

  describe("4.6") {
    it("should work"){
      // assert()
    }
  }

}