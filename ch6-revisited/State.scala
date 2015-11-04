import org.scalatest.FunSpec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rng2) = rng.nextInt
    val y = if(x < (Int.MaxValue * -1)) x + 1 else x
    val z = if(y < 0 ) { y * -1 } else y
    (z, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, rng1) = nonNegativeInt(rng)
    val y = Int.MaxValue.toDouble + 1
    (x/y, rng1)
  }

  def double_via_map(rng: RNG): Rand[Double] = {
    map(nonNegativeInt) { _/(Int.MaxValue.toDouble + 1)}
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  // def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  // def double3(rng: RNG): ((Double,Double,Double), RNG) = ???


  // 1 unit of work
  // 2 recursive step
  // 3 base cases
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0)
      return (List(), rng)
    val (i, rng1) = nonNegativeInt(rng)
    val (list, r) = ints(count - 1)(rng1)
    (i :: list, rng1)
  }

  // type Rand[+A] = RNG => (A, RNG)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    // get some thing that is type A
    // get something that is type B
    // FIRST STEP: thread the rand you pass to Rand[C] back up through ra, rb,
    // thats your starting point, and eventually the return objects contains the result
    // of processing rng a through ra, rb
    rng => {
      val (val1, r1) = ra(rng)
      val (val2, r2) = rb(r1)
      (f(val1, val2), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  }



  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (val1, r1) = f(rng)
      g(val1)(r1)
    }
  }
}

// State( s =>
//   for(
//     (a,s2) <- this(s)  # run.flatMap(a => )  O_o :((( flatmap no worky like this
//     (b,s3) <- sb.run(s2)
//   ) yield (f(a,b), s3)
// )

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State( s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a,b), s3)
    })


  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State( s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s)) // State.set(Machine) => State[Machine, Unit]
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()


List[Input] -> List[State(Machine)()]  -> List[State[Machine, (Int,Int)]] -> State[Machine, List(Int, Int)]

          Sequence(List[State[S, A]]): State[S, List[A]]

  def machineLogic(m: Machine): Machine =
    (i,m) match {
      case(Coin, Machine(true, ca, _)) if ca > 0 => Machine(false, ca, _)
      case(Turn, Machine(false, ca, co)) if ca > 0 => Machine(true, ca - 1, co + 1)
      case(_) => m
    }


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
   State(m =>
      inputs.map(i =>
        State[Machine,()].modify(machineLogic(i, m))
      )
    )
  }

  def unit[S, A](a: A) = State[S, A](s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight[State[S, List[A]]](unit(List[A]()))(
      (sa, acc) => sa.map2(acc)((a, b) => a :: b)
    )
  }
}


class StateSpec extends FunSpec {
  val rng1 = RNG.Simple(42)
  val rng2 = RNG.Simple(36)

  describe("6.1") {
    it("returns non-negative int") {
      val (x, rng2) = RNG.nonNegativeInt(rng1)
      assert(x > 0)
    }
  }
  describe("6.2 double") {
    it("returns a double between 0-1") {
      val (x, rng2) = RNG.double(rng1)
      assert(x > 0 && x < 1)
    }
  }
  describe("6.3 intdouble") {
    it("returns a int, double") {
      val ((i,d), _) = RNG.intDouble(rng1)
      assert(i.isInstanceOf[Int])
      assert(d.isInstanceOf[Double])
    }
  }
  describe("6.4 generate list of integers"){
    it("returns a list of ints"){
      val (list, _) = RNG.ints(3)(rng1)
      println(s"list: $list")
      assert(list.size == 3)
    }
  }
  describe("6.5 double via map") {
    it("returns a double between 0-1") {
      val rand = RNG.double_via_map(rng1)
      val (x, _) = rand(rng1)
      assert(x > 0 && x < 1)
    }
  }
  describe("6.6") {
    it("combines Rand[A] and Rand[B] into Rand[C]") {
      //had to use double_via_map because it uses RAND
      val y = RNG.map2(RNG.double_via_map(rng1), RNG.double_via_map(rng2))((a,b) => a + b)
      val (x, _) = y(rng1)
      assert(x < 2)
    }
  }
  describe("6.7") {
    it("sequence should work"){
      val x = RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))
      assert(x(rng1)._1 == List(1, 2, 3))
    }
    // it("sequence_match should work"){
    //   val x = RNG.sequence_match(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))
    //   assert(x(rng1)._1 == List(1, 2, 3))
    // }
  }
  describe("6.8") {
    it("flatmap should work"){
      val x = RNG.flatMap(RNG.unit(1))(a => RNG.unit(a+1))
      assert(x(rng1)._1 == 2)
    }
  }
  describe("6.11") {
    it("should sequence") {
      val x = State.sequence[RNG, Int](List(State.unit(1), State.unit(2)))
      assert(x.run(rng1)._1 == List(1,2))
    }
  }
}



