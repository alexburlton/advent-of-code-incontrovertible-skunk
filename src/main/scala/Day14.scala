import scala.collection.mutable

case class ElementAndQuantity(quantity: Long, element: String) {
  override def toString: String = s"$quantity x $element"
}
case class Reaction(product: ElementAndQuantity, units: List[ElementAndQuantity])

class Day14 extends AbstractPuzzle(14) {
  val map: mutable.HashMap[String, Reaction] = Day14Helpers.constructMap(inputLines)

  override def partA(): Any = {
    Day14Helpers.getOreToProduceFuel(1, map)
  }

  override def partB(): Any = {
    Day14Helpers.findFuelProducedByOneTrillionOre(map)
  }
}
object Day14Helpers {
  def constructMap(inputLines: List[String]): mutable.HashMap[String, Reaction] = {
    val map = mutable.HashMap[String, Reaction]()

    inputLines.foreach { line =>
      val equationSides = line.split(" => ")

      val product = parseElementAndQuantity(equationSides(1))

      val parts = equationSides.head.split(", ").map(parseElementAndQuantity).toList
      map.put(product.element, Reaction(product, parts))
    }

    map
  }

  def getOreToProduceFuel(fuelAmount: Long, map: mutable.HashMap[String, Reaction]): Long = {
    val firstStep = map("FUEL")
    var requirements = firstStep.units.map { it => ElementAndQuantity(it.quantity * fuelAmount, it.element) }
    var spares = List[ElementAndQuantity]()

    while (!requirements.forall(it => it.element == "ORE")) {
      val (reducedRequirements, stillSpare) = useUpSpares(requirements, spares)

      val newRequirements = reducedRequirements.flatMap { it => getMinimumRequirements(it, map)._1 }
      val additionalSpares = reducedRequirements.map { it => ElementAndQuantity(getMinimumRequirements(it, map)._2, it.element) }

      requirements = combineLikeElements(newRequirements)
      spares = combineLikeElements(stillSpare ++ additionalSpares)
    }

    requirements.map { it => it.quantity }.sum
  }

  private def parseElementAndQuantity(str: String): ElementAndQuantity = {
    val quantityAndElement = str.split(" ")
    ElementAndQuantity(quantityAndElement.head.toInt, quantityAndElement(1))
  }

  def getMinimumRequirements(elementAndQuantity: ElementAndQuantity, map: mutable.HashMap[String, Reaction]): (List[ElementAndQuantity], Long) = {
    if (elementAndQuantity.element == "ORE") {
      return (List(elementAndQuantity), 0)
    }

    val reaction = map(elementAndQuantity.element)

    val productProduced = reaction.product.quantity
    val productNeeded = elementAndQuantity.quantity

    val numberOfReactionsNeeded = Math.ceil(productNeeded.toDouble / productProduced.toDouble).toLong

    val requiredStuff = reaction.units.map { it => ElementAndQuantity(it.quantity * numberOfReactionsNeeded, it.element) }
    val spareProductProduced = (productProduced * numberOfReactionsNeeded) - productNeeded

    (requiredStuff, spareProductProduced)
  }

  private def combineLikeElements(value: List[ElementAndQuantity]): List[ElementAndQuantity] = {
    val map = value.groupBy(_.element)

    map.values.map { quantities =>
      val totalAmount = quantities.map { e => e.quantity }.sum
      ElementAndQuantity(totalAmount, quantities.head.element)
    }.toList
  }

  private def useUpSpares(requirements: List[ElementAndQuantity],
                           spares: List[ElementAndQuantity]): (List[ElementAndQuantity], List[ElementAndQuantity]) = {
    val newSpares = spares.map { it =>
      val required = requirements.find { req => req.element == it.element }.orNull
      if (required == null) {
        it
      } else {
        ElementAndQuantity(Math.max(0, it.quantity - required.quantity), it.element)
      }
    }

    val newRequirements = requirements.map { it =>
      val spare = spares.find { spare => spare.element == it.element }.orNull
      if (spare == null) {
        it
      } else {
        ElementAndQuantity(Math.max(0, it.quantity - spare.quantity), it.element)
      }
    }

    (newRequirements, newSpares)
  }

  /**
   * Part B
   *
   * Find how much ore produces 1,000,000 fuel, compare to
   */
  def findFuelProducedByOneTrillionOre(map: mutable.HashMap[String, Reaction]): Long = {
    var fuel = 1000000000000L / getOreToProduceFuel(1, map)

    var ore = Day14Helpers.getOreToProduceFuel(fuel, map)
    var increment = fuel / 2

    var previousDiff = 1000000000000L
    var diff = 1000000000000L - ore

    while (increment > 1) {
      if ((previousDiff compare 0) != (diff compare 0)) {
        increment = Math.ceil(increment.toDouble / 2).toInt
      }

      if (diff > 0) {
        fuel += increment
      } else {
        fuel -= increment
      }

      previousDiff = diff
      ore = Day14Helpers.getOreToProduceFuel(fuel, map)
      diff = 1000000000000L - ore
    }

    if (ore > 1000000000000L) {
      fuel - 1
    } else {
      fuel
    }
  }
}
