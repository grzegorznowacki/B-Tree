package jps

case class BTree[K, V](rootNode: Node[K, V], order: Int)(implicit keyOrdering: Ordering[K]) {

  /* This implicit method augments T with the comparison operators defined in scala.math.Ordering.Ops */
  import keyOrdering.mkOrderingOps

  def search(key: K): Option[V] = {

    def searchWithNode(node: Node[K, V]): Option[V] = {

      val foundElement = node.nodeElements.find(_.key == key)   //foundElement is of type Option[NodeElement[K, V]] because function find returns Option

      if(node.isLeaf) {
        /* Option[NodeElement] is treated as one element collection. So the underscore refers to NodeElement not Option. And after mapping we get Option[V]. */
        foundElement.map(_.value)   //If foundElement is None then after mapping we also get None.
      } else {
        foundElement match {
          case Some(element) => Some(element.value)
          case None => searchWithNode(node.nodeChildren(node.nodeElements.lastIndexWhere(key > _.key) + 1))
        }
      }
    }//searchWithNode

    searchWithNode(rootNode)
  }//search



  def insert(insertKey: K, insertValue: V): BTree[K, V] = {

    def splitNode(node: Node[K, V]): (NodeElement[K, V], Node[K, V], Node[K, V]) = {

      val returnNodeElement = node.nodeElements(order - 1)
      val returnLeftNode = Node(node.nodeElements.take(order - 1), node.nodeChildren.take(order))     //take - selects first n elements
      val returnRightNode = Node(node.nodeElements.takeRight(order - 1), node.nodeChildren.takeRight(order))    //takeRight - selects last n elements

      (returnNodeElement, returnLeftNode, returnRightNode)
    }

    def insertWithNode(node: Node[K, V]): Either[(NodeElement[K, V], Node[K,V], Node[K, V]), Node[K, V]] = {

      val foundElementIndex = node.nodeElements.indexWhere(_.key == insertKey)

      /* Key already exists */
      if(foundElementIndex > -1) {
        val newNodeElement = NodeElement(insertKey, insertValue)
        val newNodeElements = node.nodeElements.updated(foundElementIndex, newNodeElement)    // updated - A copy of this vector with one single replaced element.
        Right(node.copy(nodeElements = newNodeElements))    //copy returns new Node object with changed nodeElements field
      } else {

        val nodeCopy = if(node.isLeaf) {
          val newNodeElement = NodeElement(insertKey, insertValue)
          val newNodeElements = (node.nodeElements :+ newNodeElement).sortBy(_.key)
          node.copy(nodeElements = newNodeElements)
        } else {
          val index = node.nodeElements.lastIndexWhere(insertKey > _.key) + 1
          val childNode = node.nodeChildren(index)

          insertWithNode(childNode) match {
            case Left
          }
        }

      }

    }

  }

}

object BTree {

  def getEmptyBTree[K, V](order: Int)(implicit ordering: Ordering[K]): BTree[K, V] = {
    BTree(Node[K, V](Vector.empty, Vector.empty), order)
  }//getEmptyBTree

}
