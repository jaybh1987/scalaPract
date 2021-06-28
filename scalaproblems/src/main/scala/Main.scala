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
      case h :: tail => loop(tail, counter + 1)
      case Nil => counter
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
      case Nil       => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

//P05 (*) Reverse a list.
//  scala> reverse(List(1, 1, 2, 3, 5, 8))
//  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  
  def reverse[A](ls: List[A]): List[A] = {

    def loop[A](fstList: List[A], sndList: List[A]): List[A] = fstList match {
      case h :: tail => loop(tail, h :: sndList)
      case Nil => sndList
    }

    loop(ls, Nil)
  }

  def reverseFunctional[A](ls: List[A]): List[A] = ls.foldLeft(List[A]()) {
    (r,h) =>
      h :: r
  }

//P06 (*) Find out whether a list is a palindrome.
//  scala> isPalindrome(List(1, 2, 3, 2, 1))
//  res0: Boolean = true



//P07 (**) Flatten a nested list structure.
//  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//  res0: List[Any] = List(1, 1, 2, 3, 5, 8)



//P08 (**) Eliminate consecutive duplicates of list elements.
//  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//  res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)




}