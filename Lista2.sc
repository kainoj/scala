// Przemyslaw Joniak

// zad 1.
def take[A](n: Int, xs: List[A]) : List[A] = {
  xs match {
      case h::t => if (n > 0) h::take(n-1, t) else List()
      case Nil => Nil
  }
}

take(2, List(1,2,3,5,6)) == List(1, 2)
take(-2, List(1,2,3,5,6)) == List()
take(8, List(1,2,3,5,6)) == List(1, 2, 3, 5, 6)


// zad 2.
def drop[A](n: Int, xs: List[A]) : List[A] = {
  xs match {
      case h::tail => if (n > 0) drop(n-1, tail) else List()
      case Nil => xs
  }
}

drop(2, List(1,2,3,5,6))
drop(-2, List(1,2,3,5,6))
drop(8, List(1,2,3,5,6))


// zad. 3
// O(n^2) complexity
def reverseQ[A](xs: List[A]) : List[A] =
  xs match {
    case h::tail  => reverseQ(tail) ++ List(h)
    case _ => Nil
  }

// Linear complexity
def reverse[A](xs: List[A]) : List[A] = {

  def rev[A](xss: List[A], acc: List[A]) : List[A] =
    (xss, acc) match {
      case (Nil, _) => acc
      case (h::t, acc) => rev(t, h::acc)
    }

  rev(xs, List())
}


reverse(List(1,2,3,4,5)) == List(5,4,3,2,1)
reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(List(1)) == List(1)
reverse(List()) == List()



// zad. 4
def initSegment[A](xs: List[A], ys: List[A]) : Boolean = {
  (xs, ys) match {
    case (xh::xt, yh::yt) => (xh == yh) && initSegment(xt, yt)
    case (Nil, _) => true
    case (_, Nil) => false
  }
}

initSegment(List(1,2), List(1,2,3,4)) == true
initSegment(List(), List(1,2,3)) == true
initSegment(List(1,2), List(1,3,4)) == false
initSegment(List(1,2,3), List(1,2)) == false


// zad. 5
def replicate(xs: List[Int]) : List[Int] = {
  def replicateElem(x: Int, n: Int) : List[Int] = {
    if ( n > 0 ) List(x) ++ replicateElem(x, n-1)
    else List()
  }

  xs match {
    case h::t => replicateElem(h, h) ++ replicate(t)
    case _  => Nil
  }
}


replicate (List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(List()) == List()


// zad. 6
def root3(a: Double) : Double = {

  def abs(x: Double) : Double =
    if (x > 0) x
    else -1.0 * x

  def root(a: Double, x: Double) : Double = {
    if (abs(x*x*x - a) <= 0.000000000000001 * abs(a)) x
    else root(a, x + (a / (x*x) - x) / 3.0)

  }

  if ( a > 1) root(a, a/3)
  else root(a, a)
}


root3(8.0) == 2.0
root3(-8.0) == -2.0
