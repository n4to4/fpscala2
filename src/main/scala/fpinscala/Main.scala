package fpinscala

@main
def main(): Unit =
  println("main")

class Cafe:
  def buyCoffee(cc: CreditCard): Coffee =
    val cup = new Coffee()
    cc.charge(cup.price)
    cup

class CreditCard:
  def charge(price: Int) = ()

class Coffee:
  val price = 100
