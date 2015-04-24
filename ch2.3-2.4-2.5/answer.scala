//-------------------------------------------------------------------------------------
// exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => { (b: B) => f(a: A, b:B) }

  //Answer
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

// Questions
// Is there a difference between a => b => c vs a => { b => c}
//  Answer: I think {} represents a block of code so syntactically its valid

//-------------------------------------------------------------------------------------
// exercise 2.4
def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => (c: C)

//Answer
def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

// Notes
// f(a)(b) is syntactically equivalent to f(a,b)?  Why write it separately (a)(b) vs (a,b)?
// http://stackoverflow.com/questions/4684185/why-does-scala-provide-both-multiple-parameters-lists-and-multiple-parameters-pe
// Given that A and B are inputs it stands to reason that f takes A and B as input parameters and thus returns C
// Better explanation http://stackoverflow.com/questions/13793756/implementing-a-higher-order-function-that-performs-currying-in-scala

//-------------------------------------------------------------------------------------
// exercise 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => { f(g(a: A): B) = }

//Answer
def compose[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))