// Przemysław Joniak

// zad 1
class Pair[A, B](var fst: A, var snd: B) {
  override def toString(): String = "(" + fst + ", " + snd + ")"
}

val p = new Pair(1, "text")

p.fst = 2
p.snd = "text 2"

p.fst
p.snd


// zad 2
class BankAccount(initialBalance : Double) {
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
}

class CheckingAccount(initialBalance: Double)
  extends BankAccount(initialBalance: Double) {

    override def deposit(amount: Double): Double = super.deposit(amount - 1)

    override def withdraw(amount: Double): Double = super.withdraw(amount + 1)

}

val ca = new CheckingAccount(1000)
ca.deposit(100)
ca.checkBalance

ca.withdraw(98)
ca.checkBalance


// zad 2 b
class SavingsAccount(initialBalance: Double)
  extends BankAccount(initialBalance: Double) {

    private var transactionsPerMonth = 0

    def earnMonthlyInterest() = {
      super.deposit(0.02 * checkBalance)
      transactionsPerMonth = 0
      checkBalance
    }

    override def deposit(amount: Double): Double = {
      transactionsPerMonth += 1
      super.deposit(amount + (if(transactionsPerMonth > 3) -1 else 0))
    }

    override def withdraw(amount: Double): Double = {
      transactionsPerMonth += 1
      super.withdraw(amount + (if(transactionsPerMonth > 3) +1 else 0))
    }
}

val sa = new SavingsAccount(100)
sa.checkBalance
sa.deposit(10)
sa.deposit(20)
sa.withdraw(20)
sa.withdraw(9)
sa.earnMonthlyInterest()
sa.deposit(8)


// zad 3.
abstract class Zwierz(val imie: String = "bez imienia") {
  def rodzaj: String = getClass.getSimpleName
  def dajGlos: String

  override def toString: String = rodzaj + " " + imie + " daje glos " + dajGlos
}

class Pies(imie: String = "Bez imienia") extends Zwierz(imie: String) {
  override def dajGlos = "hau huuu"
}

class Kaczka(imie: String = "Bez imienia") extends Zwierz(imie: String) {
  override def dajGlos = "kwak"
}

val piesek = new Pies("reksio")
val kaczka = new Kaczka



object TestZwierze {
  def main(args: Array[String]){
    var zwierzata = Vector(new Pies(), new Pies("Reksio"), new Kaczka())
    for (z <- zwierzata) {
      println(z)
    }
  }
}
