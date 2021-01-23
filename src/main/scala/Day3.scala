import scala.io.Source

@main
def day3Task1() =
  val lines = Source.fromResource("day3.txt").getLines.drop(1)
  val (result, _) = lines.foldLeft((0, 3)) { (acc, line) =>
    val (trees, position) = acc
    val idx = position % line.length
    if (line(idx) == '#') (trees + 1, position + 3) else (trees, position + 3)
  }
  println(result)
