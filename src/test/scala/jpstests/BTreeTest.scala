package jpstests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import jps._

class BTreeTest extends FunSuite with BeforeAndAfter {

  test("get empty tree") {
    val bTree = new BTree[Int, Int](Node[Int, Int](Vector.empty, Vector.empty), 1)

//    val bTree = BTree.getEmptyBTree(1)    //order?

    assert(bTree.rootNode.nodeElements.isEmpty)
    assert(bTree.rootNode.nodeChildren.isEmpty)
  }

}
