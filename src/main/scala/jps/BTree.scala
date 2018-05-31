package jps

case class BTree[K, V](rootNode: Node[K, V], order: Int)(implicit keyOrdering: Ordering[K]) {

  /* This implicit method augments T with the comparison operators defined in scala.math.Ordering.Ops */

  import keyOrdering.mkOrderingOps

  def search(key: K): Option[V] = {

    def searchWithNode(node: Node[K, V]): Option[V] = {

      val foundElement = node.nodeElements.find(_.key == key) //foundElement is of type Option[NodeElement[K, V]] because function find returns Option

      if (node.isLeaf) {
        /* Option[NodeElement] is treated as one element collection. So the underscore refers to NodeElement not Option. And after mapping we get Option[V]. */
        foundElement.map(_.value) //If foundElement is None then after mapping we also get None.
      } else {
        foundElement match {
          case Some(element) => Some(element.value)
          case None => searchWithNode(node.nodeChildren(node.nodeElements.lastIndexWhere(key > _.key) + 1))
        }
      }
    } //searchWithNode

    searchWithNode(rootNode)
  } //search


  def insert(insertKey: K, insertValue: V): BTree[K, V] = {

    def splitNode(node: Node[K, V]): (NodeElement[K, V], Node[K, V], Node[K, V]) = {

      val returnNodeElement = node.nodeElements(order - 1)
      val returnLeftNode = Node(node.nodeElements.take(order - 1), node.nodeChildren.take(order)) //take - selects first n elements
      val returnRightNode = Node(node.nodeElements.takeRight(order - 1), node.nodeChildren.takeRight(order)) //takeRight - selects last n elements

      (returnNodeElement, returnLeftNode, returnRightNode)
    }

    def insertWithNode(node: Node[K, V]): Either[(NodeElement[K, V], Node[K, V], Node[K, V]), Node[K, V]] = {

      val foundElementIndex = node.nodeElements.indexWhere(_.key == insertKey)

      /* Key already exists */
      if (foundElementIndex > -1) {
        val newNodeElement = NodeElement(insertKey, insertValue)
        val newNodeElements = node.nodeElements.updated(foundElementIndex, newNodeElement) // updated - A copy of this vector with one single replaced element.
        Right(node.copy(nodeElements = newNodeElements)) //copy returns new Node object with changed nodeElements field
      } else {
        val nodeCopy = if (node.isLeaf) {
          val newNodeElement = NodeElement(insertKey, insertValue)
          val newNodeElements = (node.nodeElements :+ newNodeElement).sortBy(_.key)
          node.copy(nodeElements = newNodeElements)
        } else {
          val index = node.nodeElements.lastIndexWhere(insertKey > _.key) + 1
          val childNode = node.nodeChildren(index)

          insertWithNode(childNode) match {
            case Left((restNodeElements, splitNodeLeft, splitNodeRight)) =>
              val (left, right) = node.nodeChildren.splitAt(index) //Splits a vector into two at given position
              node.copy(
                nodeElements = (node.nodeElements :+ restNodeElements).sortBy(_.key),
                nodeChildren = (left :+ splitNodeLeft :+ splitNodeRight) ++ right.tail
              )

            case Right(modifiedChild) =>
              node.copy(nodeChildren = node.nodeChildren.updated(index, modifiedChild))
          }
        }
        if (nodeCopy.nodeElements.length == 2 * order - 1)
          Left(splitNode(nodeCopy))
        else
          Right(nodeCopy)
      }

    }

    this.copy(rootNode = insertWithNode(rootNode) match {
      case Right(node) => node
      case Left((restNodeElements, left, right)) => Node(Vector(restNodeElements), Vector(left, right))
    })
  }


  def delete(key: K): BTree[K, V] = {

    def removeLastElementFromVec(vec: Vector[Node[K, V]]): Vector[Node[K, V]] = {
      val newNodeElements = vec(vec.length - 1).nodeElements.dropRight(1)
      val lastChildrenUpdated = vec(vec.length - 1).copy(
        nodeElements = newNodeElements
      )
      vec.updated(vec.length - 1, lastChildrenUpdated)
    }

    def removeFirstElementFromVec(vec: Vector[Node[K, V]]): Vector[Node[K, V]] = {
      val newNodeElements = vec(0).nodeElements.drop(1)
      val firstChildrenUpdated = vec(0).copy(
        nodeElements = newNodeElements
      )
      vec.updated(0, firstChildrenUpdated)
    }

    //from leaf - simply delete
    def removeFromLeaf(node: Node[K, V], index: Int): Node[K, V] = {
      val newNodeElements = node.nodeElements.filter(k => node.nodeElements.indexOf(k) != index) //new nodeElements without previous index
      node.copy(nodeElements = newNodeElements)
    }

    def removeFromNonLeaf(node: Node[K, V], index: Int): Node[K, V] = {
      //if child precedes index have at least order keys, replace and remove
      if (node.nodeChildren(index).nodeElements.length >= order) {
        val lastElementInPreviousChildren = node.nodeChildren(index).nodeElements.last
        val newCurrentNode = node.nodeElements.updated(index, lastElementInPreviousChildren) //switch last element with current
        node.copy(
          nodeElements = newCurrentNode,
          nodeChildren = removeLastElementFromVec(node.nodeChildren)
        )
      } //child node, before element have less than order keys, so examine next node, and do the same with first element
      else if (node.nodeChildren(index + 1).nodeElements.length >= order) {
        val firstElemenentInNextChildren = node.nodeChildren(index + 1).nodeElements.head
        val newCurrentNode = node.nodeElements.updated(index, firstElemenentInNextChildren) //switch first child element with current
        node.copy(
          nodeElements = newCurrentNode,
          nodeChildren = removeFirstElementFromVec(node.nodeChildren)
        )
      }
      //we should merge nodes, childBefore and nextChild have less than order keys
      else {
        val nodeElementsRemoveNode = node.nodeElements.filter(x => node.nodeElements.indexOf(x) != index)
        val lastNodeChildrenElements = node.nodeChildren.last.nodeElements

        val nodeChildrenWithoutLast = node.nodeChildren.dropRight(1)
        node.copy(
          nodeElements = nodeElementsRemoveNode ++ lastNodeChildrenElements,
          nodeChildren = nodeChildrenWithoutLast
        )
      }
      //deleted element is center in node
    }

    def deleteWithNode(node: Node[K, V]): Node[K, V] = {

      val foundElementIndex = node.nodeElements.indexWhere(_.key == key) //foundElement is of type Option[NodeElement[K, V]] because function find returns Option
      if (foundElementIndex >= 0) {
        if (node.isLeaf) {
          removeFromLeaf(node, foundElementIndex)
        }
        else {
          removeFromNonLeaf(node, foundElementIndex)
        }
      }
      else {
        //we go to the end of tree, and not found
        if (node.isLeaf) {
          rootNode // not found
        } else {
          val index = node.nodeElements.lastIndexWhere(key > _.key) + 1
            node.copy(nodeChildren = node.nodeChildren.updated(index, deleteWithNode(node.nodeChildren(index))))
          } // go deeper
        }
    }

    //deleteWithNode
    this.copy(rootNode = deleteWithNode(rootNode))
  }

  //delete
}

object BTree {

  def getEmptyBTree[K, V](order: Int)(implicit ordering: Ordering[K]): BTree[K, V] = {
    BTree(Node[K, V](Vector.empty, Vector.empty), order)
  } //getEmptyBTree

}
