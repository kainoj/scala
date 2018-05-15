// Przemysław Joniak

class BoundedBuffer(N: Int) {

  private var in, out, n: Int = 0

  private val elems = new Array[Int](N)

  def put(x: Int) = this.synchronized {
    while (n >= N) {
      println(s"${Thread.currentThread.getName} waiting")
      wait
    }

    elems(in) = x ; in = (in + 1) % N ; n += 1;
    println(s"${Thread.currentThread.getName} putting $x")
    elems.foreach(e => print(s"$e ")); println

    if (n == 1) notifyAll
  }

  def take: Int = this.synchronized {

    while (n == 0) {
      println(s"${Thread.currentThread.getName} waiting");
      wait
    }

    val x = elems(out) ; elems(out) = 0 ; out = (out + 1) % N ; n -= 1
    elems.foreach(e => print(s"$e ")); println

    if (n == N-1) notifyAll
    x
  }
}


class Producer(name: String, buf: BoundedBuffer) extends Thread(name) {
  override def run: Unit =
    for(i <- 1 to 10) {
      println(s"$getName producing $i"); buf.put(i) }
  }


class Consumer(name: String, buf: BoundedBuffer) extends Thread(name) {
  override def run =
    for (i <- 1 to 10)
      println(s"$getName consumed ${buf.take}")
  }



object prodCons {

  def main(args: Array[String]) {

    val buf: BoundedBuffer = new BoundedBuffer(5)
    new Producer("Producer", buf).start
    new Consumer("Consumer", buf).start

  }
}