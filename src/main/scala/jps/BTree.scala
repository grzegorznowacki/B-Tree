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

}

object BTree {

  def getEmptyBTree[K, V](order: Int)(implicit ordering: Ordering[K]): BTree[K, V] = {
    BTree(Node[K, V](Vector.empty, Vector.empty), order)
  }//getEmptyBTree

}
