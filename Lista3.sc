// Przemyslaw Joniak

// zad. 1
def exists[A] (xs: List[A]) (p: A => Boolean) : Boolean =
  xs match {
    case Nil => false
    case h::tail => p(h) || exists(tail)(p)
  }

exists (List(5,1,2,3)) (_ == 2) == true
exists (List(2, 3, 5, 7)) (_ % 2 == 0) == true


// zad. 1 - exists + foldLeft
def exists2[A] (xs: List[A]) (p: A => Boolean) : Boolean =
  xs.foldLeft(false)((x, y) => x || p(y))


exists2 (List(5,1,2,3)) (_ == 2) == true
exists2 (List(2, 3, 5, 7)) (_ % 2 == 0) == true
exists2 (List(1, 3, 5, 7)) (_ % 2 == 0) == false


// zad. 1 - exist + foldRight
def exists3[A] (xs: List[A]) (p: A => Boolean) : Boolean =
  xs.foldRight(false)((x, y) => p(x) || y)

exists3 (List(5,1,2,3)) (_ == 2) == true
exists3 (List(2, 3, 5, 7)) (_ % 2 == 0) == true
exists3 (List(1, 3, 5, 7)) (_ % 2 == 0) == false


// zad. 2
def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  xs.foldRight(List.empty[A])((x, acc) => if (p(x)) x :: acc else acc)

filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filter (List(1, 2, 3)) (_ < 0) == List()


// zad. 3
def remove1[A](xs: List[A])(p: A => Boolean) : List[A] =
  xs match {
    case h::tail => if (p(h)) tail else h::remove1(tail)(p)
    case Nil => Nil
  }

remove1(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1(List(1,2,3)) (_ > 1 ) == List(1, 3)


// zad. 3 remove + acc
def remove2[A](xs: List[A])(p: A => Boolean) : List[A] = {

  def removeAcc(xs: List[A], acc: List[A])(p: A => Boolean) : List[A] =
    xs match {
      case h::tail => if (p(h)) (acc reverse_::: tail)
                      else removeAcc(tail, h::acc)(p)
      case Nil => Nil
    }
  removeAcc(xs, List())(p)
}

remove2(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove2(List(1,2,3)) (_ > 1 ) == List(1, 3)



// zad. 4
def splitAt[A](xs: List[A])(n: Int) : (List[A], List[A]) = {

  def splitAtAcc(xs: List[A], acc: List[A])(n: Int) : (List[A], List[A]) =
    xs match {
      case h::tail => if (n > 0) splitAtAcc(tail, h::acc)(n - 1)
                      else (acc.reverse, xs)
      case Nil => (acc.reverse, Nil)
    }
  splitAtAcc(xs, List())(n)
}

splitAt (List('a','b','c','d','e')) (2) == (List('a', 'b'), List('c', 'd', 'e'))
splitAt (List(1,2))(10) == (List(1,2), List())



// zad. 5
def mergeSort[A](xs: List[A])(less: (A, A) => Boolean): List[A] = {

  def merge(xs: List[A], ys: List[A]) : List[A] = {
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x::xt, y::yt) => if (less(x, y)) x::merge(xt, ys)
                             else y::merge(xs, yt)
    }
  }

  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (left, right) = xs.splitAt(n)
    merge(mergeSort(left)(less), mergeSort(right)(less))
  }
}

mergeSort(List(('c',1),('b',1),('c',2),('b',2),('b',3)))(_._1 <= _._1)