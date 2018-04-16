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
  def llrepeat[A](idx: Int) : Stream[A] =
    stream match {
      case p #:: xs => Stream.fill(f(idx))(p) #:::  llrepeat(idx+1)
      case Stream.Empty => stream
    }
  llrepeat(0)
}

(lrepeat (i => i+1) (Stream.from(1)) take 15).toList == List(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)
(lrepeat (i => i+1) (Stream("a", "b", "Cc"))).toList

// zad. 3 - todo
