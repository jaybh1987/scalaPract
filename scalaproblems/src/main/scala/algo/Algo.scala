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




}
