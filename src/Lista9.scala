// Przemysław Joniak

import java.util.concurrent.{Callable, Semaphore}

object Zad1b extends App {
  // Mechanizm kodu synchronizowanego

  // counter variable
  var counter = 0

  def readWriteCounter(): Unit = {
    this.synchronized {
      val incrementedCounter = counter + 1 // reading counter
      counter = incrementedCounter         // writing to counter
    }
  }

  val p = new Thread(() => for(i <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for(i <- 0 until 200000) readWriteCounter())

  p.start(); q.start()
  p.join(); q.join()
  println(s"The value of counter is $counter (synchronized)")
}

object Zad1c extends App {
  // Mechanizm semaforów

  // counter variable
  var counter = 0

  var semaphore = new Semaphore(1)

  def readWriteCounter(): Unit = {
      semaphore.acquire()
      val incrementedCounter = counter + 1 // reading counter
      counter = incrementedCounter         // writing to counter
      semaphore.release()
  }

  val p = new Thread(() => for(i <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for(i <- 0 until 200000) readWriteCounter())

  p.start(); q.start()
  p.join(); q.join()
  println(s"The value of counter is $counter (semaphore)")
}




object Lista9 {

  // Zad 2.
  def parallel[A, B](block1: => A, block2: => B): (A, B) = {

    var a : A = null.asInstanceOf[A]
    var b : B = null.asInstanceOf[B]

    val p = new Thread(() => b = block2)
    val q = new Thread(() => a = block1)

    p.start(); q.start()
    p.join(); q.join()

    (a, b)
  }

  def periodically(duration: Long, times: Int)(block: => Unit): Unit =
    if (times > 0) {
      val thr = new Thread(() => {
        block
        Thread.sleep(duration)
        periodically(duration, times - 1)(block)
      })
      thr.setDaemon(true)
      thr.start()
    }

  def main(args: Array[String]): Unit = {
    println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
    println(parallel({2+3}, {2}))
    println(parallel({2+3}, parallel({4*5}, {2})))

    println()
    println()

    periodically(1000, 5)(print("y "))
    periodically(1000, 25)(print("x "))
    Thread.sleep(10000)
    println("Done sleeping")
  }
}