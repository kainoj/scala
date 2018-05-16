// Przemysław Joniak

import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}


import scala.concurrent._
import java.util.concurrent.Semaphore
import scala.concurrent.ExecutionContext.global


class Producer(name: String, buf: BlockingQueue[Int]) extends Thread(name) {
  override def run: Unit =
    for(i <- 1 to 10) {
      println(s"$getName producing $i"); buf.put(i) }
  }


class Consumer(name: String, buf: BlockingQueue[Int]) extends Thread(name) {
  override def run =
    for (i <- 1 to 10)
      println(s"$getName consumed ${buf.take}")
  }



object prodCons {

  // Zadanie 1a + 1b

  def main(args: Array[String]) {
    val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    new Producer("Producer", buf).start
    new Consumer("Consumer", buf).start


    println("2x producers and 3x consumers")
    val buf2: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    val prods = 2
    val cons = 3
    for (i <- 1 to prods) { new Producer(s"Producer$i", buf2).start }
    for (i <- 1 to prods) { new Consumer(s"Consumer$i", buf2).start }
  }
}


object executeContextPros {

  // Zadanie 1c

  def produce(buf: ArrayBlockingQueue[Int]) = {
    for(i <- 1 to 10) { println(s"${Thread.currentThread.getName} producing $i"); buf.put(i) }
  }

  def consume(buf: ArrayBlockingQueue[Int]) = {
    for (i <- 1 to 10) println(s"${Thread.currentThread.getName} consumed ${buf.take}")
  }

  def main(args: Array[String]): Unit = {

    val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    val ctx = ExecutionContext.global

    ctx.execute(() => produce(buf))
    ctx.execute(() => consume(buf))

    Thread.sleep(500)
    println("2x producers and 3x consumers")
    val producers = 2
    val consumers = 3
    val buf2: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)

    for(i <- 1 to producers) {
      ctx.execute(() => produce(buf))
    }

    for(i <- 1 to consumers) {
      ctx.execute(() => produce(buf))
    }

    Thread.sleep(500)
  }
}

class Table(seats: Int = 5) {

  var chopstick_sem = List.fill(seats)(new Semaphore(1))

  var guard = new Semaphore(seats - 1)

  def eat(id: Int) = {
    println(s"Philosopher $id wakes up and waits to enter the dining room")

    val stick1 = id
    val stick2 = (id + 1) % seats

    guard.acquire()

    chopstick_sem(stick1).acquire()
    chopstick_sem(stick2).acquire()

    println(s"Philisopher $id is eating...")
    Thread.sleep(100)

    chopstick_sem(stick1).release()
    chopstick_sem(stick2).release()

    guard.release()
    println(s"Philosopher $id leaves the dining room and goes to sleep")
  }
}


object Philosphers {

  // Zadanie 2

  def main(args: Array[String]): Unit = {
    var philosophers = 5
    val table = new Table(philosophers)
    for( i <- 0 until philosophers) {
      println(s"Summoning philosopher $i")
      new Thread(() => for(_ <- 1 to 2) {
        println(s"Philosopher $i sleeps")
        Thread.sleep(1000)
        table.eat(i)
      }).start()
    }
  }
}