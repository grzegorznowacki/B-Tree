package jps

/**
  * This class defines BTree and supplies methods to operate on it.
  * BTree is specified by its root node and its degree.
  * All leaves are at same level.
  * A B-Tree is defined by the term minimum degree ‘degree’. The value of degree depends upon disk block size.
  * Every node except root must contain at least degree - 1 keys. Root may contain minimum 1 key.
  * All nodes (including root) may contain at most 2 * degree – 1 keys.
  * Number of children of a node is equal to the number of keys in it plus 1.
  * All keys of a node are sorted in increasing order. The child between two keys k1 and k2 contains all keys in the range from k1 and k2.
  * B-Tree grows and shrinks from the root which is unlike Binary Search Tree. Binary Search Trees grow downward and also shrink from downward.
  * Like other balanced Binary Search Trees, time complexity to search, insert and delete is O(Logn).
  *
  * @param rootNode root node of this BTree
  * @param degree minimum degree of this BTree
  * @param keyOrdering given ordering for keys
  * @tparam K type of key
  * @tparam V type of value
  */
case class BTree[K, V](rootNode: Node[K, V], degree: Int)(implicit keyOrdering: Ordering[K]) {

  /* This implicit method augments T with the comparison operators defined in scala.math.Ordering.Ops */
  import keyOrdering.mkOrderingOps

  /**
    * Search for value by a given key.
    *
    * @param key key at which value is going to be searched
    * @return found value wrapped in Option class, i.e. Some[V] if found or else None
    */
  def search(key: K): Option[V] = {

    /**
      * Auxiliary method to search for a value when a node is given.
      * Searching is done in nodeElements vector of given node, if not found this function is called recursively.
      *
      * @param node node inside which searching will be performed
      * @return found value wrapped in Option class, i.e. Some[V] if found or else None
      */
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

  /**
    * Insert key-value pair into BTree.
    * Each value is kept "under" its key.
    * Keys are unique i.e. if inserting at a key which already exists then the value is being replaced.
    *
    * @param insertKey key at which value will be inserted
    * @param insertValue inserted value
    * @return a copy of this BTree with inserted element
    */
  def insert(insertKey: K, insertValue: V): BTree[K, V] = {

    /**
      * Auxiliary method to split given node into two nodes.
      *
      * @param node given node which is supposed to be splited
      * @return a tuple consisting of NodeElement which will be put in parent node and two new nodes with appropriate elements
      */
    def splitNode(node: Node[K, V]): (NodeElement[K, V], Node[K, V], Node[K, V]) = {

      val returnNodeElement = node.nodeElements(degree - 1)
      val returnLeftNode = Node(node.nodeElements.take(degree - 1), node.nodeChildren.take(degree)) //take - selects first n elements
      val returnRightNode = Node(node.nodeElements.takeRight(degree - 1), node.nodeChildren.takeRight(degree)) //takeRight - selects last n elements

      (returnNodeElement, returnLeftNode, returnRightNode)
    }

    /**
      * Auxiliary method used by insert method.
      * It allows to insert into given node.
      *
      * @param node node into which insert will be tried
      * @return
      */
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
        if (nodeCopy.nodeElements.length == 2 * degree - 1)
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

  /**
    * Delete element by given key.
    *
    * @param key key indicating element to be deleted
    * @return a copy of this BTree with deleted element
    */
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
      if (node.nodeChildren(index).nodeElements.length >= degree) {
        val lastElementInPreviousChildren = node.nodeChildren(index).nodeElements.last
        val newCurrentNode = node.nodeElements.updated(index, lastElementInPreviousChildren) //switch last element with current
        node.copy(
          nodeElements = newCurrentNode,
          nodeChildren = removeLastElementFromVec(node.nodeChildren)
        )
      } //child node, before element have less than order keys, so examine next node, and do the same with first element
      else if (node.nodeChildren(index + 1).nodeElements.length >= degree) {
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

/**
  * Companion object defining some static methods.
  */
object BTree {

  /**
    * Generates empty BTree.
    *
    * @param degree minimum degree of this BTree
    * @param ordering given ordering for keys
    * @tparam K type of key
    * @tparam V type of value
    * @return empty BTree
    */
  def getEmptyBTree[K, V](degree: Int)(implicit ordering: Ordering[K]): BTree[K, V] = {
    BTree(Node[K, V](Vector.empty, Vector.empty), degree)
  } //getEmptyBTree

}
