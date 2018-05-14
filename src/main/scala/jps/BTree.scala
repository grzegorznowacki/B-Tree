package jps

case class BTree[K, V](rootNode: Node[K, V], order: Int)(implicit keyOrdering: Ordering[K]) {

  def search(key: K): Option[V] = {

    def searchWithNode(node: Node[K, V]): Option[V] = {

      val foundElement = node.nodeElements.find(_.key == key)   //foundElement of type Option(NodeElement)

      if(node.isLeaf) {
        foundElement.map(_.value)   //returns of type Option(Value); if foundElement is None then after mapping we also get None
      } else {
        foundElement match {
          case Some(element) => Some(element.value)
          case None => searchWithNode(node.nodeChildren(node.nodeElements.lastIndexWhere(key > _.key) + 1))
        }
      }


    }//searchWithNode

    searchWithNode(rootNode)
  }//search
  
}

}

object BTree {

  def getEmptyBTree[K, V](order: Int)(implicit ordering: Ordering[K]): BTree[K, V] = {
    BTree(Node[K, V](Vector.empty, Vector.empty), order)
  }//getEmptyBTree

}
