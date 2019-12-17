import scala.collection.mutable.ListBuffer

class ScalaWtfs {
  /**
   * Solutions:
   *  - Add 'case' to the thing
   *  - Use tuple => tuple._2 == X
   *  - val newMap = for {
   *    (key, value) <- map
   *    if value == "0" } yield (key, value)
   */
  //  def cannotResolveOverloadedMethod(): Unit = {
//    val map = mutable.HashMap[Int, String]()
//    map.put(3, "X")
//    map.put(4, "O")
//    map.put(0, "O")
//
//    val filtered = map.filter { (_: Int, value: String) => value == "O" }
//  }

  /**
   * No smart casting when taking an optional...
   */
  //  def optionalsAreShit(): Unit = {
//    val list: List[Char] = List('^', '>', '<')
//    val listBuffer = ListBuffer[Char]()
//
//    val direction = findDirection(list).orNull
//    if (direction != null) {
//      listBuffer.addOne(direction)
//    }
//  }
//  private def findDirection(list: List[Char]): Option[Char] = list.find { _ == '^' }
}
