case class Point(x: Int, y: Int, value: Int) {
  def toString(score: Int) = "(%s,%s score:%s)".format(x, y, score)
}

def sortedWithScore = points.map(p => (p, area(p).map(_.value).sum)).sortBy(_._2).reverse

def area(mid: Point) = points.filter(p => (mid.x-1 to mid.x+1).contains(p.x) && (mid.y-1 to mid.y+1).contains(p.y))

val Array(top, size, values @ _*) = args.map(_.toInt)

val points = for((value, i) <- values.zipWithIndex) yield Point(i % size, i / size, value)

println(sortedWithScore.take(top).map{ case(p,s) => p.toString(s) }.mkString)
