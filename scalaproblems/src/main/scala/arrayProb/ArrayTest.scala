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

  /*
  * Union of two given sets is the smallest set which  contains all the
  * elements of both the sets.
  *
  * A = {2,4, 5,6}
  * B = {4, 6, 7, 8}
  *
  * { 2, 4, 5, 6, 7, 8 }
  * */
  def doUnion[A](a: List[A], b: List[A]): List[A] = {
    def loop(f: List[A], s: List[A], result: List[A]): List[A] = (f, s) match {
      case (Nil, Nil)	=> result
      case (ah :: atail, Nil) if !result.contains(ah)	=> loop(atail, Nil, ah :: result)
      case (Nil, bh :: btail) if !result.contains(bh)	=> loop(Nil, btail, bh :: result)
      case (ah :: atail, bh :: btail)	=>
        if(ah != bh) loop(atail, btail, ah :: bh :: result)
        else loop(atail, btail, ah :: result)
    }
    loop(a, b, Nil)
  }


  /*
  * Given an array of size N containing only 0s, 1s, and 2s; sort the array in ascending order.
  *
  * input 0101
  * output 0011
  *
  *
  * */

  /*
  * find min
  * */

  def findMin(l: List[Int]): Int = {
    l match {
      case f :: s :: t 	if f >= s => findMin(s :: t)
      case f :: s :: t 	if f <= s => findMin(f :: t)
      case f :: s :: Nil 	if f >= s => f
      case f :: s :: Nil  if f <= s => s
      case f :: Nil => f
      case Nil => throw new Exception("Empty List")
    }
  }



  /*
  * find min max
  * */

  def loop(ls: List[Int], min: Int, max: Int): (Int, Int) = ls match {
    case h :: t =>
      if(h > min && h < max) loop(t, min, max)
      else if (h > min && h > max) loop(t, min, h)
      else if (h < min && h < max) loop(t, h, max)
      else loop(t, min, max)
    case Nil => (min, max)

  }

  def minMax(l: List[Int]): (Int, Int) = l match {
    case f :: s :: t => loop(t, f, s)
    case f :: Nil => throw new Exception("Invalid Input")
    case Nil => throw new Exception("Empty list")
  }

}




















