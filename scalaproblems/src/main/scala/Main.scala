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


  def encode[A](ls: List[A]): List[(Int, A)] = pack(ls).map{ e => (e.length, e.head)}

  def encodeModified[A](ls: List[A]): List[Any] = encode(ls).map{
    t =>
        if(t._1 == 1) t._2 else t
  }

  def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] = encode(ls).map {
    t =>
      if(t._1 == 1) Left(t._2) else Right(t)
  }

//  //p12
//  def decode[A](ls: List[(Int, A)]): List[A] = ls.flatMap {
//    e =>
//        make(e._1, e._2)
//  }


  //p14
  def duplicate[A](ls: List[A]): List[A] = ls.flatMap( r => List(r, r))

  /*
  * input List('a', 'b')
  * */
  //p15
  def duplicateN[A](n: Int, ls: List[A]): List[A] = ls.flatMap {
    r =>
      (0 until n).flatMap( _ => List(r))
  }

  //p16
  def dropNth[A](ls: List[A], idx: Int): List[A] = ls
    .zipWithIndex
    .filter(r => r._2 != idx)
    .map(_._1)


  def dropKth[A](ls: List[A], inx: Int): List[A] = (ls, inx) match {
    case (Nil, _)       => Nil
    case (_ :: tail,  0) => dropKth(tail, 0 - 1)
    case (h :: tail, n) => h :: dropKth(tail, n - 1)
  }

  //P17
  def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
    case (_, Nil)       => (Nil, Nil)
    case (0, list)      => (Nil, list)
    case (n, h :: tail) =>
      val (pre, post) = splitRecursive(n - 1, tail)
      (h :: pre, post)
  }
  def splitFunction[A](ls: List[A], i: Int) = ls.splitAt(i)

  //p18

  def sliceRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = (start, end, ls) match {
    case (_, _, Nil)  => Nil
    case (_, e, _) if e <= 0   => Nil
    case (s, e, h :: tail)  if s <= 0 => h :: sliceRecursive(0, e - 1, tail)
    case (s, e, _ :: tail)    =>  sliceRecursive(s - 1, e - 1, tail)
  }

  def slice[A](s: Int, e: Int, ls: List[A]): List[A] = {
    def loop(counter: Int, ls: List[A]): List[A] = (counter, ls) match {
      case (_, Nil)							=> Nil
      case (n, _ :: tail)	if n < s 			=> loop(n + 1, tail)
      case (n ,h :: tail) if n == s || n <= e => h :: loop(n + 1, tail)
      case (n, _) if n > e => Nil
      case (_, _) => Nil
    }
    loop(0, ls)
  }

  def sliceBuiltin[A](start: Int, end: Int, ls: List[A]): List[A] = ls.slice(start, end)


  //P19
  /*
  * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  * */
  def rotate[A](n: Int, l: List[A]): List[A] = {
    def loop(counter: Int, ls: List[A], rotated: List[A]): List[A] = (counter, ls, rotated) match {
      case (c, h :: tail, l)  if c != 0 => loop(c - 1, tail, h :: l)
      case (0, l, r) => l ++ r
    }
    loop(n, l, Nil)
  }

  def rotate2[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if(ls.isEmpty) 0 else n % ls.length
    if(nBounded < 0) rotate2(nBounded + ls.length, ls) else (ls drop nBounded) ::: (ls take nBounded)
  }



}
















