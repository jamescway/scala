import org.scalatest.FunSpec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  // Int.MaxValue  Int.MinValue
  // 2147483647    -2147483648
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (y, nextRNG) = rng.nextInt
    (if(y < 0) y + 1 * -1 else y, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, nextRNG) = nonNegativeInt(rng)
    ((x+1).toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nextRNG1) = nonNegativeInt(rng)
    val (d, nextRNG2) = double(nextRNG1)
    ((i,d), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (x, nextRng) = rng.nextInt
    val newCnt = count - 1

    val lizst = if(newCnt > 0){
      val (l, rng2) = ints(newCnt)(nextRng)
      l :+ x
    }
    else {
      List(x)
    }
    (lizst, nextRng)
  }

  def double_with_map(rng: RNG): Rand[Double] =
    map(nonNegativeInt) { _ / (Int.MaxValue.toDouble + 1) }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def sequence_match[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case (h :: t) => map2(h, sequence(t))(_ :: _)
      case Nil => unit(Nil)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Strip off the rand
  // convert it into Rand[B]
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  // take a nonNegativeInt
  //  high i, low n   10000000  1        mod: 1     res: 10000000
  //  low i, high n   1         10000000 mod: 1     res: 10000000
  //  low i, low n    1         10000000
  //  high i, high n  10000000  10000000 mod: 1000000 res: 123213
  //  low i  high -n  10000     99999999 mod: 10000   res: negative number
  def nonNegativeIntLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if(i + (n - 1) - mod >= 0) unit(mod) else nonNegativeIntLessThan(n)
    }
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a,b))
      }
    }
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  // items --> item --> rules --> output

  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

  //   //sequence expects a List of states not a list of inputs
  //   sequence(inputs).flatMap(a => a.map( b => b match
  //     {
  //       case Coin => { println("coin...") }
  //       case Turn => { println("turn...") }
  //     }
  //   ))
  // }

}

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    })))
    s <- get
  } yield (s.coins, s.candies)
}

class StateSpec extends FunSpec {
  val rng1 = RNG.Simple(42)
  val rng11 = RNG.Simple(43)

  describe("6.1") {
    it("non-negative int should work"){
      val (x, rng2) = RNG.nonNegativeInt(rng1)
      assert(x > 0)
    }
  }
  describe("6.2") {
    it("double 0 to 1 should work"){
      val (x, rng2) = RNG.double(rng1)
      assert(x > 0 && x < 1)
    }
  }
  describe("6.3") {
    it("intDouble should work"){
      val ((i,d), r) = RNG.intDouble(rng1)
      assert(i.isInstanceOf[Int])
      assert(d.isInstanceOf[Double])
    }
    it("DoubleInt should work"){
      val ((d,i), r) = RNG.doubleInt(rng1)
      assert(i.isInstanceOf[Int])
      assert(d.isInstanceOf[Double])
    }
    it("Double3 should work"){
      val ((d1, d2, d3), r) = RNG.double3(rng1)
      assert(d1.isInstanceOf[Double])
      assert(d2.isInstanceOf[Double])
      assert(d3.isInstanceOf[Double])
    }
  }
  describe("6.4") {
    it("ints should work"){
      val (list, r) = RNG.ints(3)(rng1)
      println(list.toString)
      assert(list.length == 3)
    }
  }
  describe("6.5") {
    it("double with map should work"){
      val y = RNG.double_with_map(rng1)
      val (x, _) = y(rng1)
      assert(x > 0 && x < 1)
    }
  }
  describe("6.6") {
    it("map2 should work"){
      val y = RNG.map2(RNG.double_with_map(rng1), RNG.double_with_map(rng11))((a,b) => a + b)
      val (x, _) = y(rng1)
      assert(x < 2)
    }
  }
  describe("6.7") {
    it("sequence should work"){
      val x = RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))
      assert(x(rng1)._1 == List(1, 2, 3))
    }
    it("sequence_match should work"){
      val x = RNG.sequence_match(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))
      assert(x(rng1)._1 == List(1, 2, 3))
    }
  }
  describe("6.8") {
    it("flatMap should work"){
      val y = RNG.flatMap(RNG.unit(1))(RNG.unit(_))
      val (x, _) = y(rng11)
      assert(x < 2)
    }
    it("nonNegativeIntLessThan recursive case should work"){
      val (x, _) = RNG.nonNegativeIntLessThan(-99999999)(rng1)
      assert(x > 0)
    }
    it("nonNegativeIntLessThan regular case should work"){
      val (x, _) = RNG.nonNegativeIntLessThan(1000)(rng1)
      assert(x > 0)
    }
  }
  describe("6.9") {
    it("map with flatmap should work"){
      val y = RNG.mapWithFlatMap(RNG.unit(1))(i => i + 1)
      val (x, _) = y(rng1)
      assert(x == 2)
    }
    it("map2 with flatmap should work"){
      val y = RNG.map2WithFlatMap(RNG.double_with_map(rng1), RNG.double_with_map(rng11))((a,b) => a + b)
      val (x, _) = y(rng1)
      assert(x < 2)
    }
  }
  describe("6.10") {
    it("State.unit should work") {
      val func_that_should_returns_tuple = State.unit[RNG,Int](1)
      val a = func_that_should_returns_tuple.run(rng1)
      assert(a._1 == 1)
    }
    it("State.unit2 should work") {
      val func_that_should_returns_tuple = State.unit[Int,Int](2)
      val (a,b) = func_that_should_returns_tuple.run(3)
      assert(a == 2 && b == 3)
    }
    it("state.map should work") {
      // val state = new State(State.unit(rng1))
      // val x = state.map(_ + 1)
      // val (y, _) = x(rng11)
      // assert(y == 2)
    }
  }
  describe("6.11") {
    it("candy state machine should work") {
      val x = Candy.simulateMachine(List(Coin, Turn))
      println(x.run(Machine(true, 5,5)))

    }
  }
}











