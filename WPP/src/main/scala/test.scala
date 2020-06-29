object test {
  def main(args: Array[String]): Unit = {
    val prob = new Pour.Pouring(Vector(4, 9, 19))
    println(prob.solution(17))
    //println(prob.allPaths.take(3).toList)
  }
}
