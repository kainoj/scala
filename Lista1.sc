// Przemyslaw Joniak

// zad. 1
def suma(xs: List[Double]): Double = {
  if (xs == Nil) 0
  else xs.head + suma(xs.tail)
}

suma(List(1.0, 2.0, 3.0))
suma(List())
suma(List(-1.0))


// zad. 2
def ends[A](xs: List[A]): (A, A) = {
  require(xs.nonEmpty, "The list cannot be empty")

  def last[A](ys: List[A]): A = {
    val yHead :: yTail = ys
    if (yTail == Nil) yHead
    else last(yTail)
  }

  if (xs.tail == Nil) (xs.head, xs.head)
  else (xs.head, last(xs.tail))
}

ends(List(1,2,3,4,6))
ends(List(1))


// zad. 3
def posortowana(xs: List[Int]) : Boolean =
  if (xs == Nil || xs.tail == Nil) true
  else {
    val first :: (reszta@(second :: tail)) = xs
    if (first <= second) posortowana(reszta)
    else false
  }

posortowana(List())
posortowana(List(1))
posortowana(List(1,2,2,3,4))
posortowana(List(1,2,4,3))


// zad. 4
def glue(xs: List[String], sep: String) : String = {
  require(xs != Nil, "oops, the list is empty")

  val first::tail = xs
  if (tail == Nil) first
  else {
    val second::tail2 = tail
    if (tail2 == Nil) first + sep + second
    else first + sep + glue(tail, sep)
  }
}

glue(List("This"), "-")
glue(List("This", "is"), "-")
glue(List("This", "is", "a very long"), "-")
glue(List("This", "is", "a very long", "list"), "-")


// zad. 5
def split(xs: List[Char], x: Char): (List[Char], List[Char]) =
  if (xs == Nil) (Nil, Nil)
  else
    if (xs.head <= x) (xs.head :: split(xs.tail, x)._1, split(xs.tail, x)._2)
    else (split(xs.tail, x)._1, xs.head :: split(xs.tail, x)._2 )

split(List('a', 's', 'h', 'g'), 'g')


// zad. 6
def swap[A](xs: List[A], i: Int): List[A] =
  xs match {
    case _ if (i <= 0) => xs
    case Nil => Nil
    case h1::h2::tail if tail == Nil && i > 2 => xs
    case h::tail => swap(tail, i - 1) ++ List(h)
  }


swap(List("a", "b", "5", "6"), 2)
swap(List("a", "b"), 20)
swap(List("a"), 3)
swap(Nil, 200)