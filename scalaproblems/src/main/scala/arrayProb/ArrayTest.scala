package arrayProb

class ArrayTest {

  def move(ls: List[Int]): List[Int] = {
    def loop(l: List[Int]): (List[Int], List[Int]) = l match {
      case h :: tail =>
        val (l, r) = loop(tail)
        if(h < 0) (h :: l, r) else (l, h :: r)
      case Nil => (Nil, Nil)
    }
    val (l,r) = loop(ls)
    r ::: l
  }

  def peak[A](n: Int, ls: List[A]): A = ls match {
    case h :: _ if n == 0 => h
    case h :: tail => peak(n - 1, tail)
  }
  /*
  * Given an unsorted array A of size N that contains only non-negative integers, find a continuous sub-array which adds to a given number S.
  * Input:
          N = 5, S = 12
          A[] = {1,2,3,7,5}
          Output: 2 4
          Explanation: The sum of elements
          from 2nd position to 4th position
          is 12.
  * */


  import scala.util.control.Breaks._

  def findIndex(t: Int, ls: List[Int]) = {

    var flag = false

    def loop(s: Int, e: Int): Unit = {
      var sum = 0 ;
      for(i <- s to e) {
        sum = sum + ls(i)
        if(sum == t) {
          flag = true;
          print(s, e)
        } else print("")
      }
    }

    breakable {
      for(i <- 0 until ls.length; from = i + 1) {
        for(j <- from until ls.length) {

          if(flag) break
          else loop(i, j)
        }
      }
    }
  }




}




















