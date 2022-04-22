package algo

sealed class Node[T](var data: T, next: Node[T])  {
  def getData: T = data
  def getNext: Node[T] = this.next
  def printList(): Unit = {
    print(data)
    if(next != null) {
      print(",")
      next.printList()
    }
  }
}

case class LinkedList[T]() {
  var head: Node[T] = null

}