package jpstests

import org.scalatest.FunSuite
import jps._

/**
  * This class supplies tests for BTree.
  */
class BTreeTest extends FunSuite {

  test("GIVEN empty btree with degree 3 WHEN do nothing THEN it should be empty") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)    //order = 3

    assert(bTree.rootNode.nodeElements.isEmpty)
  }

  test("GIVEN empty btree with degree 3 WHEN do nothing THEN root node should not have children") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    assert(bTree.rootNode.nodeChildren.isEmpty)
  }

  test("GIVEN empty btree with degree 3 WHEN insert one element THEN it should no longer be empty") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5)   // Do not forget that bTree is immutable. Insert does not insert into it but produces new one.

    assert(newBTree.rootNode.nodeElements.isEmpty == false)
  }

  test("GIVEN empty btree with degree 3 WHEN insert one element THEN root node should be leaf") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5)

    assert(newBTree.rootNode.isLeaf)
  }

  test("GIVEN empty btree with degree 3 WHEN insert one element THEN root node should not have children") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5)

    assert(newBTree.rootNode.nodeChildren.isEmpty)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 4 different elements THEN root node should not have children") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5).insert(3, 5).insert(4, 5).insert(5, 5)

    assert(newBTree.rootNode.nodeChildren.isEmpty)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 5 different elements THEN root node should have 2 children") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(11, 5).insert(3, 5).insert(4, 5).insert(1, 5).insert(5, 5)

    assert(newBTree.rootNode.nodeChildren.length == 2)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 4 different elements THEN root node should have 4 elements") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5).insert(3, 5).insert(4, 5).insert(5, 5)

    assert(newBTree.rootNode.nodeElements.length == 4)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 2 same-key elements THEN root node should have 1 element") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5).insert(2, 6)

    assert(newBTree.rootNode.nodeElements.length == 1)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 1 element with key X value Y THEN after search with key X value Y should be returned") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5)

    assert(newBTree.search(2).get == 5)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 2 same-key elements THEN element value should be updated") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(2, 5).insert(2, 6)

    assert(newBTree.search(2).get == 6)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 6 different elements THEN after search by key value should be returned") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(13, 5).insert(3, 6).insert(6, 2).insert(5, 8).insert(2, 8).insert(7, 9)

    assert(newBTree.search(6).get == 2)
  }


  test("GIVEN empty btree with degree 3 WHEN insert one delete one") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(13, 5).delete(13)

    assert(newBTree.rootNode.nodeElements.isEmpty)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 2 delete one") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(13, 5).insert(14, 6).delete(14)

    assert(newBTree.search(14).isEmpty)
  }

  test("GIVEN empty btree with degree 3 WHEN insert 6 delete one") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(1,1).
      insert(2,1).
      insert(3,1).
      insert(4,1).
      insert(5,1).
      insert(6,1).
      delete(6)

    assert(newBTree.search(6).isEmpty)
  }
  test("GIVEN empty btree with degree 3 WHEN insert 8 delete ") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(1,1).
      insert(2,1).
      insert(3,1).
      insert(4,1).
      insert(5,1).
      insert(6,1).
      insert(7,1).
      insert(8,1).
      delete(4)

    assert(newBTree.search(4).isEmpty)
  }
  test("GIVEN empty btree with degree 3 WHEN insert 8 delete from child") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(1,1).
      insert(2,1).
      insert(3,1).
      insert(4,1).
      insert(5,1).
      insert(6,1).
      insert(7,1).
      insert(8,1).
      delete(7)

    assert(newBTree.search(7).isEmpty)
  }
  test("GIVEN empty btree with degree 3 WHEN insert 3 delete one and found other element") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(1,1).
      insert(2,1).
      insert(3,1).
      delete(3)

    assert(newBTree.search(2).get == 1)
  }
  test("GIVEN value from btree with degree 3 WHEN insert 8 delete other child (check if delete dont delete other values)") {
    val bTree = BTree.getEmptyBTree[Int, Int](3)

    val newBTree = bTree.insert(1,1).
      insert(2,1).
      insert(3,1).
      insert(4,1).
      insert(5,1).
      insert(6,1).
      insert(7,1).
      insert(8,1).
      delete(8)

    assert(newBTree.search(5).get == 1)
  }
}
