// Przemyslaw Joniak

import reflect.ClassTag

class FullException(msg: String) extends Exception(msg)
abstract class MyQueueMut[E] {
  @throws[FullException]
  def enqueue(x: E): Unit
  def dequeue: Unit
  @throws[NoSuchElementException]
  def first: E
  def isEmpty: Boolean
  def isFull: Boolean
}

class QueueMut[E: ClassTag](val n: Int = 1000) extends MyQueueMut[E] {

  private val q: Array[E] = new Array[E](n)
  private var f, r  = 0 : Int

  override def enqueue(x: E): Unit =
    if (this.isFull)
      throw new FullException("Queue is full")
    else {
      q(r) = x
      r = (r + 1) % n
    }


  override def dequeue: Unit =
    if (this.isEmpty)
      ()
    else
      f = (f + 1) % n


  override def first: E =
    if (this.isEmpty)
      throw new NoSuchElementException("Queue is empty")
    else
      q(f)

  override def isEmpty: Boolean =
    f == r

  override def isFull: Boolean =
    f == ((r + 1) % n)

  override def toString: String = {
    q.toList.mkString(", ") + "     f = " + f + "  r = " + r
  }
}

object QueueMut {

  def empty[E: ClassTag](n: Int = 1000) : QueueMut[E] = new QueueMut[E](n)

  def apply[E: ClassTag](xs: E*): QueueMut[E] = {
    var q : QueueMut[E] = QueueMut.empty()
    for(x <- xs)
      q.enqueue(x)
    q
  }
}


object Lista8 {

  def main(args: Array[String]): Unit = {
    var q  = new QueueMut[Int](3)
    var q2 : QueueMut[Int] = QueueMut.empty(3)
    q.enqueue(1)
    q.enqueue(2)
    println(q.first)    // 1
    println(q)          // 1, 2
    q.dequeue
    println(q)          // 2
    q.enqueue(3)
    println(q)          // 2, 3
    q.dequeue           // q = (3)
    q.enqueue(4)        // q = (3, 4)
    println(q.first)    // 3

    var q3 = QueueMut(1,2,3)
    println(q3.first)
  }
}