package listFunctions

object FunctionApplication {


  //++ concate two lists

  //++: prepend all add elements back

  //+: add element in list to left

  //:++ append list

  //:: add element in the begenning.

  //::: add elements of given list in front of list

  //addString

  //addString (stringBuilder, seperator string)

  //appended all list at the end.

  //appendall

  //apply get element on specific index.

  val l :List[Double] = List(1,2,3,4,5,6)
  val even = new PartialFunction[Double, Double] {
    override def isDefinedAt(x: Double): Boolean = x != 0

    override def apply(v1: Double): Double = if(v1 % 2 == 0) v1 else 0
  }

  val d: PartialFunction[Int, Int] = {
    case x if x % 3 == 0 => x * 3
  }

  val m: PartialFunction[Int, Int] = {
    case x if (x % 5) == 0 => x * 5
  }

  val m2: PartialFunction[Int, Int] = {
    case y if (y % 2 ) == 0 => y * 2
  }
  m.orElse(m2)

  val j: PartialFunction[Int, Int] = {
    case x if x % 2 == 0 => x * 5
  }

  //collect
  //collect first
  //combinations
  //compose
  //compose

  List(1,2,3,4,5).collect(j)









}













