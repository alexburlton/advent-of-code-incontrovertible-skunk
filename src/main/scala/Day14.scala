import scala.collection.mutable

class Day14 extends AbstractPuzzle(14) {
  val map: mutable.HashMap[String, Reaction] = constructMap()

  override def partA(): Any = {
    getOreToProduceFuel(1)
  }

  override def partB(): Any = {
    val ore = getOreToProduceFuel(4200533)

    ore
  }

  private def getOreToProduceFuel(fuelAmount: Long) = {
    val firstStep = map("FUEL")
    var requirements = firstStep.units.map { it => ElementAndQuantity(it.quantity * fuelAmount, it.element) }
    var spares = List[ElementAndQuantity]()
    while (!requirements.forall(it => it.element == "ORE")) {
      val (reducedRequirements, stillSpare) = useUpSpares(requirements, spares)

      val newRequirements = reducedRequirements.flatMap { it => getMinimumRequirements(it)._1 }
      val additionalSpares = reducedRequirements.map { it => ElementAndQuantity(getMinimumRequirements(it)._2, it.element) }

      requirements = combineLikeElements(newRequirements)
      spares = combineLikeElements(stillSpare ++ additionalSpares)
    }

    requirements.map { it => it.quantity }.sum
  }

  private def constructMap(): mutable.HashMap[String, Reaction] = {
    val map = mutable.HashMap[String, Reaction]()

    inputLines.foreach { line =>
      val equationSides = line.split(" => ")

      val product = parseElementAndQuantity(equationSides(1))

      val parts = equationSides.head.split(", ").map(parseElementAndQuantity).toList
      map.put(product.element, Reaction(product, parts))
    }

    map
  }

  private def parseElementAndQuantity(str: String): ElementAndQuantity = {
    val quantityAndElement = str.split(" ")
    ElementAndQuantity(quantityAndElement.head.toInt, quantityAndElement(1))
  }

  private def getMinimumRequirements(elementAndQuantity: ElementAndQuantity): (List[ElementAndQuantity], Long) = {
    if (elementAndQuantity.element == "ORE") {
      return (List(elementAndQuantity), 0)
    }

    val reaction = map(elementAndQuantity.element)

    val productProduced = reaction.product.quantity
    val productNeeded = elementAndQuantity.quantity

    val numberOfReactionsNeeded = Math.ceil(productNeeded.toDouble / productProduced.toDouble).toInt

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

  private def useUpSpares(
                           requirements: List[ElementAndQuantity],
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

  case class ElementAndQuantity(quantity: Long, element: String)
  case class Reaction(product: ElementAndQuantity, units: List[ElementAndQuantity])
}
