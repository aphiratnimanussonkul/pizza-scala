package pizza

import scala.collection.mutable.ArrayBuffer

class Pizza(
    var crustSize: CrustSize,
    var crustType: CrustType,
    val toppings: ArrayBuffer[Topping]
) {

  def addTopping(t: Topping): Unit = { toppings += t }
  def removeTopping(t: Topping): Unit = { toppings -= t }
  def removeAllToppings(): Unit = { toppings.clear() }

  override def toString(): String = {
    val price = getPrice()
    val toppingsString = for (t <- toppings) yield t
    s"""Pizza:
           |  Crust Size: $crustSize
           |  Crust Type: $crustType
           |  Toppings: $toppingsString
           |  Price: $price
         """.stripMargin
  }

  def getPrice(): Int = {
    getTotalToppingPrice() + getCrustSizePrice() + getCrustTypePrice()
  }

  def getCrustSizePrice(): Int = {
    crustSize match {
      case SmallCrustSize  => 100
      case MediumCrustSize => 150
      case LargeCrustSize  => 200
    }
  }

  def getCrustTypePrice(): Int = {
    crustType match {
      case ThinCrustType    => 0
      case RegularCrustType => 0
      case ThickCrustType   => 10
    }
  }

  def getToppingPrice(topping: Topping): Int = {
    topping match {
      case Cheese    => 15
      case Pepperoni => 5
      case Sausage   => 5
      case Mushrooms => 10
      case Onions    => 5
    }
  }

  def getTotalToppingPrice(): Int = {
    toppings.map(getToppingPrice).foldLeft(0)(_ + _)
  }
}
