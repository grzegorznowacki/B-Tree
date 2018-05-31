package jps


object Main {
  def main(args: Array[String]): Unit = {
    println("Hello")

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
  }
}

