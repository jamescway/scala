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

  def &&(Prop: p): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[N]] = {
    Gen(State(RNG.sequence(List.fill(n)(g.sample) ))

  }
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
}

trait SGen[+A] {

}

case class Gen[A](sample: State[RNG,A]) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }



}

class GenSpec extends FunSpec {

  describe("8.1") {
    it(""){

      assert(1 > 0)
    }
  }

}