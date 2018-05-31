package jps

/**
  * This class defines a node of BTree.
  * Node stores information in a vector about NodeElements it contains.
  * Node stores information in a vector about its children.
  *
  * @param nodeElements Vector of NodeElements which are stored in this node
  * @param nodeChildren Vector of node's children
  * @param ordering given ordering for keys
  * @tparam K type of key
  * @tparam V type of value
  */
case class Node[K, V](nodeElements: Vector[NodeElement[K, V]], nodeChildren: Vector[Node[K, V]])(implicit ordering: Ordering[K]) {
  /**
    * Checks if this node is a leaf.
    *
    * @return true if this node is a leaf, else false
    */
  def isLeaf: Boolean = nodeChildren.isEmpty
}
