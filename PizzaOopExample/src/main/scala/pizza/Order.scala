package pizza

import scala.collection.mutable.ArrayBuffer

class Order(
    val pizzas: ArrayBuffer[Pizza],
    var customer: Customer
) {

  def addPizza(p: Pizza): Unit = {
    pizzas += p
  }

  def removePizza(p: Pizza): Unit = {
    pizzas -= p
  }

  def getBasePrice(): Int = {
    val pizzaPrices = pizzas.map(_.getPrice)
    return pizzaPrices.foldLeft(0)(_ + _)
  }
  def getTaxes(): Int = (getBasePrice() * 0.07).toInt
  def getTotalPrice(): Int = {
    getBasePrice() + getTaxes()
  }

  def printOrder(): Unit = {
    for (p <- pizzas) {
      println(p)
    }
  }

}
