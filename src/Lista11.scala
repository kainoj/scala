// Przemysław Joniak

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object Lista11 {

  // Zadanie 1a.
  def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    fut1 zip fut2


  // Zadanie 1b.
  def pairFut2[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for {
      a <- fut1
      b <- fut2
    } yield (a, b)


  implicit class FutureOps[T](val self: Future[T]) {

    // Zadanie 2a
    def exists(p: T => Boolean): Future[Boolean] = self map p // todo - jak nie propaowac niepoofzdenias

    // Zadanie 2b
    def exists_b(p: T => Boolean): Future[Boolean] = {
      val prom = Promise[Boolean]
      self onComplete {
        case Success(value) => prom.success(p(value))
        case Failure(error) => prom.success(false)
      }
      prom.future
    }
  }

  def main(args: Array[String]): Unit = {

    // Zadanie 1
    val pair = pairFut(Future {"ok"}, Future{123 + 23})
    val pair2 = pairFut(Future {"fail"}, Future{1/0})
    Thread.sleep(500)
    println(pair.value)
    println(pair2.value)
    println()

    // Zadanie 2
    val fut1_positive = Future {1}.exists(_ > 0)
    val fut1_negative = Future {-1}.exists(_ > 0)
    val fut2_positive = Future {1}.exists_b(_ > 0)
    val fut2_negative = Future {"text"}.exists_b((p: String) => p.length() > 30)

    Thread.sleep(500)
    println(fut1_positive.value)
    println(fut1_negative.value)
    println(fut2_positive.value)
    println(fut2_negative.value)
  }
}


object Zad3 extends App {

  import scala.concurrent.{ExecutionContext, Future, Promise}
  import ExecutionContext.Implicits.global
  import scala.util.{Failure, Success}

  implicit class PromiseOps[T](val self: Promise[T]) {

    def compose[S](f: S => T): Promise[S] = {
      val prom = Promise[S]

      prom.future.onComplete {
        case Success(value) => self.success(f(value))
        case Failure(err) =>  self.failure(err)
      }
      prom
    }
  }

  // przykładowy test
  val pT = Promise[String]
  val pS: Promise[Int] = pT.compose(int => s"val = $int")


  Future {
    Thread.sleep(1000)
    pS.success(1)
    // pS.failure(new Exception)
  }

  pT.future onComplete {
    case Success(result) => println(result)
    case Failure(cause) => println(s"Failed with $cause")
  }

  Thread.sleep(2000)
}



import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import scala.io.Source

object WordCount {

  def main(args: Array[String]) {
    val path = "test_11/" //ścieżka do folderu; można ją oczywiście wczytać"

    val promiseOfFinalResult = Promise[Seq[(String, Int)]]

    // Oblicz promiseOfFinalResult
    promiseOfFinalResult.future onComplete {
      case Success(result) => result foreach println
      case Failure(t) => t.printStackTrace
    }

    val futureOfProcessFiles = Promise[Seq[(String, Int)]]
    futureOfProcessFiles.future onComplete {
      case Success(succ) => promiseOfFinalResult.success(succ)
      case Failure(err) => println(s"processFiles() error: $err")
    }

    scanFiles(path) onComplete {
      case Success(fileNames) => {
        processFiles(fileNames) onComplete {
          case Success(succ) => futureOfProcessFiles.success(succ)
          case Failure(err) => print(err)
        }
      }
      case Failure(err) => println(s"errpr reading files: $err")
    }


    Thread.sleep(5000)
  }

  // Oblicza liczbę słów w każdym pliku z sekwencji wejściowej
  private def processFiles(fileNames: Seq[String]): Future[Seq[(String, Int)]] = {
      Future.sequence(fileNames.map(fname => processFile(fname)))
  }
  // Wskazówka. Wykorzystaj Future.sequence(futures)

  // Oblicza liczbę słów w podanym pliku i zwraca parę: (nazwa pliku, liczba słów)
  private def processFile(fileName: String): Future[(String, Int)] = {
    val f = Source.fromFile(fileName)
    try Future{(fileName, f.getLines().foldLeft(0)((acc, x) => acc + x.split(" ").length)) } // finally f.close()
  }

  // Zwraca sekwencję nazw plików (w naszym przypadku Array[String])
  private def scanFiles(docRoot: String): Future[Seq[String]] =
    Future { new java.io.File(docRoot).list.map(docRoot + _) }
}