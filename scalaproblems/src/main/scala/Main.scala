import java.util.NoSuchElementException

object Main extends App {

//P01 (*) Find the last element of a list.
// scala> last(List(1, 1, 2, 3, 5, 8))
//  res0: Int = 8

  def findLast[A](l: List[A]): A = l match {
    case h :: tail => findLast(tail)
    case h :: Nil => h
    case Nil => throw new NoSuchElementException
  }

//P02 (*) Find the last but one element of a list.
//  scala> last(List(1, 1, 2, 3, 5, 8))
//  res0: Int = 8

  def findLastButOne[A](l: List[A]): A = l match {
    case h :: _ :: Nil => h
    case h :: tail => findLastButOne(tail)
    case Nil => throw new NoSuchElementException
  }



//P03 (*) Find the Kth element of a list.
//  scala> nth(2, List(1, 1, 2, 3, 5, 8))
//  res0: Int = 2

  def findkth(k: Int, l: List[Int]): Int = {

    def loop(ls: List[Int], counter: Int): Int = ls match {
      case h :: tail if(counter == k) => h
      case h :: tail if(counter != k ) => loop(tail, counter + 1)
      case Nil => throw new NoSuchElementException
    }

    loop(l, 1)
  }

  def findkth[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (0, h :: _) => h
    case (n, _ :: tail) => findkth(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }


//P04 (*) Find the number of elements of a list.
//  scala> length(List(1, 1, 2, 3, 5, 8))
//  res0: Int = 6

  def findLength[A](l: List[A]): Int = {

    def loop[A](ls: List[A], counter: Int): Int = ls match {
      case h :: tail      =>  loop(tail, counter + 1)
      case Nil            =>  counter
    }
    loop(l, 0)
  }

  def lengthRecursive[A](ls: List[A]): Int = ls match {
    case h :: tail => 1 + lengthRecursive(tail)
    case Nil => 0
  }

  def f(i: Int): Int = i + 1

  // Tail recursive.
  def reverseTailRecursive[A](ls: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil              =>       result
      case h :: tail        =>       reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

//P05 (*) Reverse a list.
//  scala> reverse(List(1, 1, 2, 3, 5, 8))
//  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  
  def reverse[A](ls: List[A]): List[A] = {

    def loop[A](fstList: List[A], sndList: List[A]): List[A] = fstList match {
      case h :: tail        =>  loop(tail, h :: sndList)
      case Nil              =>  sndList
    }

    loop(ls, Nil)
  }

  def reverseFunctional[A](ls: List[A]): List[A] = ls.foldLeft(List[A]()) ((r,h) => h :: r)

//P06 (*) Find out whether a list is a palindrome.
//  scala> isPalindrome(List(1, 2, 3, 2, 1))
//  res0: Boolean = true



//P07 (**) Flatten a nested list structure.
//  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//  res0: List[Any] = List(1, 1, 2, 3, 5, 8)



//P08 (**) Eliminate consecutive duplicates of list elements.
//  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//  res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  List('a', 'b', 'b', 'c' , 'c')

  def dropConsecutive(l: List[Char]): List[Char] = l match {
    case h  ::  s   ::  tail  if h != s    =>   h :: dropConsecutive(s :: tail)
    case h  ::  s   ::  tail  if h == s    =>   dropConsecutive(s :: tail)
    case h  :: Nil                         =>   h :: dropConsecutive(Nil)
    case Nil                               =>   Nil
  }
  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => h :: compressRecursive( tail.dropWhile(_ == h) )
  }
  def compressFunctional[A](ls: List[A]): List[A] = ls.foldRight(List[A]()) {
    case (h, r) =>
      if(r.isEmpty || r.head != h) h :: r else r
  }

  def last[A](seq: Seq[A]): A = seq match {
    case h :: Nil           =>    h
    case _ :: tail          =>    last(tail)
    case _                  =>    throw new NoSuchElementException("Not Found")
  }


  /*
  * P09 Pack consecutive duplicates of list element into sublist
  *   pack(List('a','a', 'b', 'c', 'c')
  *   output : List( List('a', 'a'), List('b'), List('c','c') )
  *
  *   what span function doing
  *     span function take the element from the list and check with predicate
  *     if true then it move forward but if false then it do not move forward.
  *
  * */

  def pack[A](ls: List[A]): List[List[A]] = {
    if(ls.isEmpty)  List(List())
    else {
      val (packed, next)  = ls.span( _ == ls.head)
      if(next == Nil ) List(packed)
      else packed :: pack(next)
    }
  }



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

  def fixQue(x: List[Int], elm: Int, size: Int): List[Int] = (x.length, size) match {
    case (len, sz) if len < sz      =>      x :+ elm
    case (len, sz) if len > sz      =>      (x :+ elm).drop( (len - sz) + 1)
    case (len, sz) if len == sz     =>       x.drop(1) :+ elm
    case _                          =>       Nil
  }

  def fun(element: Int, ls: List[Int]): List[Int] = ls match {
    case h :: tail  if  element > h     =>      h :: fun(element, tail)
    case h :: tail  if  element < h     =>      element :: fun(h, tail)
    case Nil => element :: Nil
  }

  def sorted(l: List[Int]): List[Int] = l match {
    case h :: tail      =>    fun(h, sorted(tail))
    case Nil            =>    Nil
  }

  sealed trait Json
  object Json {
    case class Str(s: String) extends Json
    case class Num(value: Double) extends Json
  }

  //def convertToJson(x)
  def convertToJson(x: Any): Json = x match {
    case s: String => Json.Str(s)
    case d: Double => Json.Num(d)
    case i: Int => Json.Num(i.toDouble)
  }
  //61409
}

case class Person(id: Int, name: String)
object Person {
  def apply(name: String) : Person = new Person(1, name)
}


















