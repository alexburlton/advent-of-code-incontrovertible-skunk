import scala.collection.mutable

class DAG
{
  private val hmNodeToChildren = scala.collection.mutable.Map[String, mutable.Set[String]]()

  def addNode(node: String)
  {
    hmNodeToChildren.put(node, mutable.Set[String]())
  }

  def addLink(parent: String, child: String): Boolean = {
    //Check we're not creating a cycle
    if (relationshipExists(child, parent))
    {
      return false
    }

    if (parent == child)
    {
      return false
    }

    if (!hmNodeToChildren.contains(parent))
    {
      addNode(parent)
    }

    val set: mutable.Set[String] = hmNodeToChildren(parent)
    val result = set.add(child)

    if (!hmNodeToChildren.contains(child))
    {
      addNode(child)
    }

    result
  }

  def getAllChildren(parent: String): mutable.Set[String] = {
    val children = getChildren(parent)
    if (children == null) return mutable.Set[String]()

    val allChildren = mutable.Set[String]()
      children.foreach { child =>
      allChildren.add(child)
      allChildren.addAll(getAllChildren(child))
    }

    allChildren
  }


  private def relationshipExists(parent: String, child: String): Boolean = {
    if (!hmNodeToChildren.contains(parent))
    {
      return false
    }

    val list = getAllChildren(parent)
    list.contains(child)
  }

  private def getChildren(node: String, depth: Int = 1): mutable.Set[String] =
  {
    val children = hmNodeToChildren.get(node).orNull

    if (children == null) return null

    if (depth == 0)
    {
      return mutable.Set[String](node)
    }

    val childrenAtRightDepth = mutable.Set[String]()
    children.foreach{ child =>
      val subChildren = getChildren(child, depth-1)
        childrenAtRightDepth.addAll(subChildren)
    }

    childrenAtRightDepth
  }

  def getDepth(node: String): Int = {
    val children = hmNodeToChildren.get(node).orNull
    if (children == null) return -1
    if (children.isEmpty) return 0

    1 + children.map{ child => getDepth(child) }.max
  }

  def sumChildCountForAllNodes(): Int =
    hmNodeToChildren.keys.toList.map { key =>
      getAllChildren(key).size
    }.sum

  def getShortestPathBetween(nodeA: String, nodeB: String): Int = {
    var setA = getAllLinkedNodes(nodeA)
    var setB = getAllLinkedNodes(nodeB)

    //Keep track of these as an optimisation
    val nodesAlreadyVisited = mutable.Set[String]()

    var steps = 0
    var overlap = setA.intersect(setB)
    while (overlap.isEmpty) {
      nodesAlreadyVisited.addAll(setA)
      nodesAlreadyVisited.addAll(setB)

      val newSetA = expandSetToAllLinks(setA).diff(nodesAlreadyVisited)
      val newSetB = expandSetToAllLinks(setB).diff(nodesAlreadyVisited)

      overlap = newSetA.intersect(newSetB)
      setA = newSetA
      setB = newSetB

      steps += 2
    }

    steps
  }

  private def expandSetToAllLinks(set: Set[String]): Set[String] = {
    val newSet = mutable.Set[String]()
    set.foreach { node => newSet.addAll(getAllLinkedNodes(node)) }
    newSet.toSet
  }

  private def getAllLinkedNodes(node: String): Set[String] = {
    val children = getChildren(node)
    val parents = getParents(node)

    children.addAll(parents)
    children.toSet
  }

  def getParents(node: String): Set[String] = {
    hmNodeToChildren.filter { it => it._2.contains(node) }.keys.toSet
  }
}