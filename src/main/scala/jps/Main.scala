package jps

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello")
    val bTree1 = new BTree[Int, Int](Node[Int, Int](Vector.empty, Vector.empty), 1)
    val bTree2 = BTree.getEmptyBTree(1)
  }
}

