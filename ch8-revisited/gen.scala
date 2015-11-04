import org.scalatest.FunSpec

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State.boolean)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))


  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nonNegativeIntLessThan(stopExclusive).map(n => start + n % (stopExclusive - start))


}

// case class State[S,A](run: S => (A,S)).
case class Gen[A](sample: State[RNG,A]) {
  def map[A,B](f: A => B): Gen[B] = ???

  def flatMap[A,B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =


  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A]

}


// trait Gen[A] {

// }

trait SGen[+A] {

}


class GenSpec extends FunSpec {
  describe("8.5 - unit") {

  }

}
