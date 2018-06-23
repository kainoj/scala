import scala.concurrent._
import java.util.concurrent.Semaphore
import scala.concurrent.ExecutionContext.global

import java.lang.Thread


object zadanie1 {

  import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

  // Zadanie 1a + 1b

  def main(args: Array[String]) {
    val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    new Producer("Producer", buf).start
    new Consumer("Consumer", buf).start

    val buf2: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    val number = 4
   
    val producers = {
      for (i <- 1 to number)
        yield new Producer(s"Producer$i", buf2) 
    }

    val consumers = {
      for (i <- 1 to number)
        yield new Consumer(s"Consumer$i", buf2) 
    }

    val startTime = System.nanoTime
    
    producers.foreach(t => t.start())
    consumers.foreach(t => t.start())
    producers.foreach(t => t.join())
    consumers.foreach(t => t.join())

    val estimatedTime = (System.nanoTime - startTime)/1000000
    println(s"it took $estimatedTime miliseconds")
  }


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



}





object zadanie2 {

  class Producer(name: String, buf: BoundedBuffer) extends Thread(name) {
    override def run: Unit =
      for (i <- 1 to 10) {buf.put(i)}
  }

  class Consumer(name: String, buf: BoundedBuffer) extends Thread(name) {
    override def run =
      for (i <- 1 to 10) buf.take
  }

  class BoundedBuffer(N: Int) {

  private var in, out, n: Int = 0
  private val elems = new Array[Int](N)
  
  def put(x: Int) = this.synchronized {
    while (n >= N) { wait }
    elems(in) = x ; in = (in + 1) % N ; n += 1
    // elems.foreach(e => print(s"$e "));
    if (n == 1) notifyAll
  }

  def take: Int = this.synchronized {
    while (n == 0) {wait}
    val x = elems(out) ; elems(out) = 0 ; out = (out + 1) % N ; n -= 1
    // elems.foreach(e => print(s"$e ")); println
    if (n == N-1) notifyAll
    x
  }
}


  def main(args: Array[String]) {
    val buf: BoundedBuffer = new BoundedBuffer(5)

    val producers = {
      for (i <- 1 to number)
        yield new Producer(s"Producer$i", buf2) 
    }

    val consumers = {
      for (i <- 1 to number)
        yield new Consumer(s"Consumer$i", buf2) 
    }

    val startTime = System.nanoTime
    
    producers.foreach(t => t.start())
    consumers.foreach(t => t.start())
    producers.foreach(t => t.join())
    consumers.foreach(t => t.join())

    val estimatedTime = (System.nanoTime - startTime)/1000000
    println(s"it took $estimatedTime miliseconds")
  }
}