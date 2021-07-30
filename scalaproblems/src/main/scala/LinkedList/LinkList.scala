package LinkedList

case class LinkList[T]() {
  var head: Node[T] = null
}

sealed class Node[T](var data: T, var next: Node[T]) {

  def getData: T = this.data

  def getNext: Node[T] = this.next

  def printList(): Unit = {
    print(data)

    if(next != null) {
      print(",")
      next.printList()
    }
  }
}
