// Przemyslaw Joniak

import akka.actor._

class Server(bound : Int) extends Actor {

  val N = scala.util.Random.nextInt(bound)

  println(s"Guess my number from the interval [0..$bound] (spoiler: it's $N!)")

  def receive = {
    case Server.M(guess) => {
      if (guess == N) sender ! Client.R("equal")
      if (guess > N) sender ! Client.R("bigger")
      if (guess < N) sender ! Client.R("lower")
    }
    case x => throw new Exception(s"Server: invalid message: $x")
  }
}

object Server {
  def props = Props[Server]
  case class M(res: Int)
}

class Client(name: String, server: ActorRef, bound: Int) extends Actor {

  println(s"$name starts with upper bound $bound")

  var a = 0
  var b = bound
  var guess = (a + b) / 2

  def receive = {
    case Client.R(msg) => msg match {
      case "equal" => println(s"$name: I guessed it! $guess")
      case "bigger" => {
        b = guess
        guess = (a + b) /2
        println(s"$name too big. Trying: $guess")
        sender ! Server.M(guess)
      }
      case "lower" => {
        a = guess
        guess = (a + b) /2
        println(s"$name too big. Trying: $guess")
        sender ! Server.M(guess)
      }
    }
    case Client.Start => server ! Server.M(guess)
    case x => throw new Exception(s"Client: invalid message: $x")
  }
}

object Client {
  def props = Props(classOf[Client], Main.server)
  case class R(response: String)
  case object Start
}


object Main extends App {
  val bound = 100

  // ActorSystem is a heavy object: create only one per application
  val ourSystem = ActorSystem("MySystem")
  val server: ActorRef = ourSystem.actorOf(Props(classOf[Server], bound))

  val client1: ActorRef = ourSystem.actorOf(Props(classOf[Client], "Client1", server, bound))
  val client2: ActorRef = ourSystem.actorOf(Props(classOf[Client], "Client2", server, bound))

  client1 ! Client.Start
  client2 ! Client.Start

  Thread.sleep(1000)
  ourSystem.terminate
}