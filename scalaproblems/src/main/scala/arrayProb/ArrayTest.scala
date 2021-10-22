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
}
