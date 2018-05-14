package jps

case class Node[K, V](nodeElements: Vector[NodeElement[K, V]], nodeChildren: Vector[Node[K, V]])(implicit ordering: Ordering[K]) {
  def isLeaf: Boolean = nodeChildren.isEmpty
}
