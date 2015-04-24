// object Hi {
//   def main(args: Array[String]) = println("Hi!")
// }

import com.twitter.util.{Future,Promise}

object Main extends App {
  def func(): Future[Future[Nothing]] = {
    Future.value(Future.exception(new Exception("doh")))
  }

  try{
    val future_exception = func
    // future_exception.get onFailure { e =>
    //   println("......." + e.toString)
    // }
    for {
      i <- future_exception.onFailure(e => throw e)
    } yield i
  } catch {
    case e: Throwable => println("****************Exception: " + e.toString)
  }
}