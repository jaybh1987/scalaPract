package algo

import scala.annotation.tailrec

object Algo {

  def updateList[A](r: Seq[A], elementToAdd: A): Seq[A] = r.dropRight(1) :+ elementToAdd

  def runLengthEncoding(x: String): String = {
    x.foldLeft(Seq[(Int, Char)]()) {
      (myDefault, element) =>
        if(myDefault.isEmpty) myDefault :+ (1, element)
        else {
          val (c, char_value)   =   myDefault.last
          if(char_value == element) updateList(myDefault, (c + 1, element) )
          else myDefault :+ (1, element)
        }
    }.map{ case (count, alpha) => s"$count$alpha"}.mkString
  }

  def getcount[A](l: List[A], count: Int): Int = l match {
    case _ :: tail => getcount(tail, count + 1)
    case Nil => count
  }

  trait Animal
  case class Dog(x: String) extends Animal
  case class Cat(y: String) extends Animal


  def fixQue(x: List[Int], elm: Int, size: Int): List[Int] = (x.length, size) match {
    case (len, sz) if len < sz      =>      x :+ elm
    case (len, sz) if len > sz      =>      (x :+ elm).drop( (len - sz) + 1)
    case (len, sz) if len == sz     =>       x.drop(1) :+ elm
    case _                          =>       Nil
  }

  def helpSort(element: Int, ls: List[Int]): List[Int] = ls match {
    case h :: tail  if  element > h     =>      h :: helpSort(element, tail)
    case h :: tail  if  element < h     =>      element :: helpSort(h, tail)
    case Nil => element :: Nil
  }

  def sorted(l: List[Int]): List[Int] = l match {
    case h :: tail      =>    helpSort(h, sorted(tail))
    case Nil            =>    Nil
  }

  def merge[A <% Ordered[A]](xs: List[A]): List[A] = {
    def fun(xs: List[A], ys: List[A]): List[A] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x :: xs1, y :: ys1) =>
        if(x < y) x :: fun(xs1, ys) else y :: fun(xs, ys1)
    }
    val n = xs.length / 2
    if(n == 0) xs
    else {
      val (left, right) = xs.splitAt( n )
      fun(  merge(left) ,   merge (right) )
    }
  }



  /*
  * merge(List(4, 3, 89, 90, 45, 12))
  *
  * val ( List(4, 3, 89), List(90, 45, 12) )
  * fun( merge(List(4, 3, 89)), merge( List(90 45, 12) )
  * fun( fun( merge(List(4), merge(List(3, 89) ), fun( merge(List(90), merge(List(45, 12)) ) )
  * fun( fun( fun()        , fun()              , fun(  fun(),         fun() ) ) )
  * */


  def tables(x: Int): Unit = {
    val c = 1
    def prepare(x: Int, counter: Int): Unit = {
      if(counter == 11) ()
      else {
        println(s"$x * $counter = ${x * counter}")
        prepare(x, counter + 1)
      }
    }
    prepare(x, c)
  }

  def swap[A](a: Array[A], i: Int, j: Int):Unit = {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def partition(a: Array[Int], low: Int, high: Int): Int = {
    val pivot = a(high)
    var i = low - 1
    for(j <- low to high) {
      if(a(j) < pivot) {
        i+=1
        swap(a, i, j)
      }
    }
    i+=1
    swap(a, i, high)
    i
  }

  def quicksort(a: Array[Int], low: Int, high: Int): Unit = {
    if(low < high) {
      val pi = partition(a, low, high)
      quicksort(a, low, pi - 1)
      quicksort(a, pi + 1, high)
    }
  }


  def factorial(n: BigInt): BigInt =  {
    @tailrec
    def loop(num: BigInt, result: BigInt): BigInt = {
      if(num == 1) result
      else loop(num - 1, num * result)
    }
    loop(n, 1)
  }

  def intersect(x: List[Int], y: List[Int]): List[Int] = {
    var j : List[Int] = List.empty
    for(i <- y.indices) {
      if(x.contains(y(i))) {
        j = j:+y(i)
      } else ()
    }
    j
  }


  def logorithmOfNumberBase2(number: Int): Int = {
    def loop(n: Int, c: Int): Int = {
      if(n == number) c
      else loop(n * 2, c + 1)
    }
    if(number % 2 == 0) loop(2, 1) else -1
  }





  //  Input:
//    n = 7

//  arr[] = {1, 5, 3, 4, 3, 5, 6}
//  Output: 2
//  Explanation:
//    5 is appearing twice and
//  its first appearence is at index 2
//  which is less than 3 whose first
//  occuring index is 3.

  def firstRepeatingElem[A](l: List[A]): Option[Int] = {
    var m: Map[A, Int] = Map.empty
    var ans: Option[Int] = None
    for(i <- l.indices) {
      if(m.contains(l(i))) {
        ans = m.find{
          case (k, v) => k == l(i)
        }.map(_._2)
      } else {
        m = m + ( l(i) -> i)
        None
      }
    }
    ans
  }

  def printFibbo(n: Int) : Unit = {
    def loop(a: Int, b: Int, end: Int): Unit = {
      if(b > end) println(" end ")
      else {
        print(s" $b ")
        loop(b, a + b, end)
      }
    }
    loop(0, 1, n)
  }


  /*
  * find first non repeating elment from the list.
  *
  * need to change this function.
  * */
//  def firstNonRepeating[A](l: List[A]): String = {
//    var m: Map[A, Int] = Map.empty
//
//    for(i <- l.indices) {
//      if(m.contains( l(i))) {
//        var c = m(l(i)) + 1
//        m = m + ( l(i) -> c)
//      } else m = m + ( l(i) -> 1)
//    }
//    m.collect {
//      case (k, v) if v == 1 => (k, v)
//    }.keys.mkString
//  }

  def firstNonRepeating(l: List[Int]): String = {
    var m: Map[Int, Int] = Map.empty
    for(i <- l.indices) {
      if(m.contains( l(i) )) {
        val count = m(l(i)) + 1
        m = m + ( l(i) -> count)
      } else m = m + ( l(i) -> 1)
    }
    m.collect {
      case (k, v) if v == 1 => (k, v)
    }.keys.mkString
  }

  /*
  * Given an array of positive and negative numbers.
  * Find if there is a subarray (of size at-least one) with 0 sum.
  *
  *
  * Input:
    5
    4 2 -3 1 6

    Output:
    Yes

    Explanation:
    2, -3, 1 is the subarray
    with sum 0.
  * */

  val l = List(4,2,-3,1,6)

  def subArraySumZero(l: List[Int]): List[Int] = {
    def loop(ls: List[Int],result: List[Int]): List[Int] = ls match {
      case h :: t if result == Nil => loop(t, result :+ h)
      case h :: tail => if( result.sum == 0 ) result else loop(tail, result :+ h)
      case Nil => result
    }
    def loop2(r: List[Int]): List[Int] = {
      val l = loop(r, Nil)
      if(l.sum != 0) loop2(l.drop(1)) else l
    }
    loop2(l)
  }

  def findLogestSeq(list: List[Int]) = {

    def f(l: List[Int]): List[Int] = {
      import scala.util.control.Breaks._
      var counter = 0
      var arr: List[Int] = List.empty
      breakable {
        for(i <- l.indices) {
          if(i == 0) {
            counter = l(i)
          }
          if(l(i) == counter) arr = arr :+ l(i) else break
          counter = counter + 1
        }
      }
      arr
    }

    def loop(l: List[Int], res: List[(List[Int],Int)]): List[(List[Int], Int)] = {
      if(l == Nil) res
      else {
        val out = f(l)
        loop( l.diff(out), res :+ (out, out.length) )
      }
    }

    loop(list, List((List.empty[Int], 0)))
      .foldRight(List.empty[Int]) {
        case ( (mylist, c), result) =>
          if(result.length > c) result else mylist
      }
  }



}















