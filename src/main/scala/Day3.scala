import scala.io.Source

def traverse(lines: IterableOnce[String], right: Int, down: Int) =
  val (result, _) = lines.iterator
    .drop(down)
    .zipWithIndex
    .collect { case (str, i) if i % down == 0 => str }
    .foldLeft((0L, right)) { (acc, line) =>
      val (trees, position) = acc
      val idx = position % line.length
      if (line(idx) == '#') (trees + 1, position + right) else (trees, position + right)
    }
  result

@main
def day3Task1() =
  val lines = Source.fromResource("day3.txt").getLines.toSeq
  val result = traverse(lines, 3, 1)
  println(result)

@main
def day3Task2() =
  val lines = Source.fromResource("day3.txt").getLines.toSeq
  def fun = traverse(lines, _, _)
  val result = fun(1, 1) * fun(3, 1) * fun(5, 1) * fun(7, 1) * fun(1, 2)
  println(result)
