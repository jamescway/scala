import org.scalatest.FunSpec

object MyModule {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if(as.length == 1) return true

    val head = as.head
    val tail = as.tail
    val second = tail.head

    if(ordered(head, second)){
      isSorted(as.tail, ordered)
    }
    else{
      return false
    }
  }

  // def main(args: Array[String]) {
  //   val result = isSorted[Int](Array(1,2,3,4,5,6), (i: Int, j: Int) => { i < j } )
  //   println(s"IsSorted? $result")
  // }
}

class IsSortedSpec extends FunSpec {

  describe("#isSorted") {
    it("should work here"){
      assert(MyModule.isSorted[Int](Array(1,2,3,4,5,6), (i: Int, j: Int) => { i < j } ) == true)
    }
    it("should and here"){
      assert(MyModule.isSorted[Int](Array(1,2,3,4,5,6), (i: Int, j: Int) => { i < j } ) == true)
    }
  }
}

// [1,2,3]

//          head  tail  second
// [1,2,3]  1     2,3   2
// [2,3]    2     3     3
// [3]      3     []    []