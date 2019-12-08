import scala.collection.mutable.ListBuffer

class DayEight extends AbstractPuzzle(8) {
  private val layerSize = 25 * 6

  override def partA(): Int = {
    val layers = parseLayersFromInput()

    val zeroCounts = layers.map { layer => layer.count(_ == '0') }
    val min = zeroCounts.min
    val ixWithMin = zeroCounts.indexOf(min)

    val layerWithMin = layers(ixWithMin)
    layerWithMin.count { _ == '1' } * layerWithMin.count { _ == '2' }
  }

  override def partB(): Int = {
    val layers = parseLayersFromInput()

    val renderedPixels = ListBuffer[String]()
    for (i <- layers.head.indices) {
      val pixels = layers.map { _(i) }
      val colour = pixels.filterNot { _ == '2' }.head
      renderedPixels.addOne(toPixel(colour))
    }

    println("")
    val pixelLayers = renderedPixels.grouped(25)
    pixelLayers.foreach { layer =>
      val colours = layer.mkString("")
      println(colours)
    }
    println("")

    0
  }

  private def parseLayersFromInput(): List[List[Char]] = {
    val imagePixels = inputLines.head.toList
    imagePixels.grouped(layerSize).toList
  }

  private def toPixel(c: Char): String =
    c match {
      case '0' => " "
      case '1' => "X"
    }
}
