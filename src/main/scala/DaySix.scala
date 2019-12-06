class DaySix extends AbstractPuzzle(6) {
  override def partA(): Int = {
    val dag = DaySixHelpers.constructDag(inputLines)
    dag.sumChildCountForAllNodes()
  }

  override def partB(): Int = {
    val dag = DaySixHelpers.constructDag(inputLines)
    dag.getShortestPathBetween("YOU", "SAN")
  }
}

object DaySixHelpers {
  def constructDag(lines: List[String]): DAG = {
    val dag = new DAG()
    lines.foreach { line =>
      val parentAndChild = line.split(')')
      dag.addLink(parentAndChild(1), parentAndChild.head)
    }

    dag
  }
}
