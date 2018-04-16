// Przemyslaw Joniak

class MyQueue[+A] private (val xs: List[A], val ys: List[A]) {

  def isEmpty[A] : Boolean = xs.isEmpty

  def empty: MyQueue[A] = new MyQueue(List(), List())

  def first() : A = xs.head

  def enqueue[B >: A](elem: B): MyQueue[B] =
    if (xs.nonEmpty)
      new MyQueue(xs, elem::ys)
    else
      new MyQueue(List(elem), ys)


  def dequeue() : MyQueue[A] =
    xs match {
      case h::b::t => new MyQueue(b::t, ys)
      case h::Nil => new MyQueue(ys.reverse, List())
      case _ => this
    }

  def firstOption() : Option[A] = {
    xs match {
      case h::tail => Some(h)
      case _ => None
    }
  }

  override def toString: String = (xs ++ ys.reverse) mkString ", "
}


object MyQueue {

  def empty() = new MyQueue(List(), List())

  def apply[A](xs: A*): MyQueue[A] = new MyQueue(xs.toList, List())
}


sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]



object Lista7 {

  def main(args: Array[String]): Unit = {
    
    // Queue
    var q = MyQueue(1, 2, 3)

    println(q.first()) // 1
    q = q.dequeue()
    println(q.first()) // 2
    q = q.dequeue()
    println(q.first()) // 3
    println(q.firstOption()) // Some(3)
    q = q.dequeue()
    println(q.isEmpty)       // True

    println(q.firstOption()) // None
    
    // BFS
    val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
    println(breadthBT(t))
  }


  def breadthBT[A](tree: BT[A]) : List[A] =  {
    
    def breadth(toVisit: MyQueue[BT[A]]) : List[A] = 
      toVisit.firstOption() match {
        case Some(Node(elem, left, right)) => {
          elem :: breadth(toVisit.dequeue().enqueue(left).enqueue(right))
        }
        case Some(Empty) => breadth(toVisit.dequeue())
        case _ => Nil
      }
    breadth(MyQueue(tree))
  }
}