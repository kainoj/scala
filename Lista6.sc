// Przemyslaw Joniak

// zad. 1
def whileLoop(cond: => Boolean)(expr: => Unit) : Unit= {
  if (cond) {
    expr
    whileLoop(cond)(expr)
  }
}

var count = 0

whileLoop(count < 4) {
  println(count)
  count += 1
}


// zad. 2
def lrepeat[A] (f: Int => Int)(stream: Stream[A]) : Stream[A] = {
  def llrepeat[A](idx: Int)(stream: Stream[A]) : Stream[A] =
    stream match {
      case p #:: xs => Stream.fill(f(idx))(p) #:::  llrepeat(idx+1)(xs)
      case Stream.Empty => stream
    }
  llrepeat(0)(stream)
}

(lrepeat (i => i+1) (Stream.from(1)) take 15).toList == List(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)
(lrepeat (i => i+1) (Stream("a", "b", "Cc"))).toList == List("a", "b", "b", "Cc", "Cc", "Cc")



sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]


// zad. 3a
def lBreadth[A](ltree: lBT[A]) : Stream[A] = {

  def llBreadth[A](q: List[lBT[A]]) : Stream[A] = {
    q match  {
      case LEmpty :: t => llBreadth(t)
      case LNode(elem, left, right)::t => elem #:: llBreadth(t ++ List(left(), right()))
      case Nil => Stream.Empty
    }
  }
  llBreadth(List(ltree))
}

// Drzewo z wykładu 4.
val t2 =  LNode(1,
  () => LNode(2,
    () => LNode(4,
      () => LEmpty, () => LEmpty),
    () => LEmpty),
  () => LNode(3,
    () => LNode(5,
      () => LEmpty,
      () =>LNode(6,
        () =>LEmpty,() => LEmpty)),
    () => LEmpty))


lBreadth(t2).toList == List(1, 2, 3, 4, 5, 6)


// zad. 3b
def lTree (n: Int) : lBT[Int] = {
  LNode(n, () => lTree(2*n), () => lTree(2*n + 1))
}

lBreadth(lTree(1)).take(10).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
lBreadth(lTree(2)).take(7).toList == List(2, 4, 5, 8, 9, 10, 11)

