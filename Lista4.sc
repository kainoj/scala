// Przemyslaw Joniak

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)

// zad. 1
def sumBT[A](bt: BT[Int]) : Int =
  bt match {
    case Node(v, left, right) => v + sumBT(left) + sumBT(right)
    case Empty => 0
  }

sumBT(t) == 6
sumBT(Empty) == 0
sumBT(Node(1, Empty, Node(12, Empty, Empty))) == 13


// zad. 2
def foldBT[A, B](f: A => ((B, B) => B))(acc: B)(bt: BT[A]) : B =
    bt match {
      case Node(v, left, right) => f(v)(foldBT(f)(acc)(left), foldBT(f)(acc)(right))
      case Empty => acc
    }


// zad. 3 a
def sumBTfold[A](bt: BT[Int]) : Int =
  foldBT((x: Int) => (y: Int, z: Int) => x+y+z)(0)(bt)

sumBTfold(t) == 6


// zad. 3 b
def inorderBTfold[A](bt: BT[A]) : List[A] =
  foldBT((x: A) => (y: List[A], z: List[A]) => y++(x::z))(List())(bt)


inorderBTfold(t) == List(2, 3, 1)

val t2 = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,
         Node(6,Empty,Empty)),Empty))

inorderBTfold(t2) == List(4, 2, 1, 5, 6, 3)


// zad. 4
def mapBT[A, B](f: A => B)(tree: BT[A]) : BT[B] =
  foldBT[A, BT[B]](x => (y, z) => Node(f(x), y, z))(Empty)(tree)


mapBT((v: Int) => 2 * v)(t: BT[Int]) == Node(2,Node(4,Empty,Node(6,Empty,Empty)),Empty)

// zad 5. - todo