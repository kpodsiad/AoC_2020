import scala.io.Source

def binarySearch(start: Int, to: Int, reguls: List[Boolean]): Int = {
  val (result, _) = reguls.foldLeft((start, to)) { (acc, lookUp) =>
    val (min, max) = acc
    val pivot = min + (max - min) / 2
    if (lookUp) (pivot + 1, max)
    else (min, pivot)
  }
  result
}

def lineToId(line: String): Int = {
  val mappedLine = line.toList.map {
    case 'B' | 'R' => true
    case 'F' | 'L' => false
  }
  val row = binarySearch(0, 127, mappedLine.take(7))
  val column = binarySearch(0, 7, mappedLine.takeRight(3))
  // id = row * 8 + column
  row * 8 + column
}

@main
def day5Task1() =
  val lines = Source.fromResource("day5.txt").getLines()
  val result = lines.foldLeft(0) { (acc, line) =>
    val id = lineToId(line)
    acc.max(id)
  }
  println(result)

@main
def day5Task2() =
  val lines = Source.fromResource("day5.txt").getLines()
  val ids = lines
    .foldLeft(List.empty[Int]) { (acc, line) =>
      val id = lineToId(line)
      id :: acc
    }.toSet
  
  val maybeId = ids.foldLeft(Option.empty[(Int)]) { (acc, id) =>
    acc match {
      case some@Some(_) =>
        some
      case None =>
        if (ids(id - 2) && !ids(id - 1)) Some(id - 1)
        else if (ids(id + 2) && !ids(id + 1)) Some(id + 1)
        else acc
    }
  }
  maybeId.foreach(println)
