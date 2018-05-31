package jps


object Main {
  def main(args: Array[String]): Unit = {
    println("Hello")

    def filter(list: List[Int], condition: Int => Boolean): List[Int] =
      for (item <- list if condition(item))
        yield item


  }
}

