import scala.io.Source

def parseLine(line: String) =
  val splitted = line.split(":")
  val rangeAndLetter = splitted(0).split(" ")
  val range = rangeAndLetter(0).split("-")
  (range(0).toInt, range(1).toInt, rangeAndLetter(1)(0), splitted(1).stripLeading())

/**
 * Verify if password contains the given letter. 
 * <password> must contains beetween <letter> at least <min> and at most <max> times
 * <min>-<max> <letter>: <password>
 * 2-4 p: vpkpp
 * 2-10 b: bfbbbbcbnpbbbbt
 */
@main
def day2Task1() =
  val lines = Source.fromResource("day2.txt").getLines
  val result = lines.foldLeft(0) { (acc, line) =>
    val (min, max, letter, password) = parseLine(line)
    val letterCount = password.groupBy(identity).collectFirst {
      case (c, str) if c == letter => str.length
    }.getOrElse(0)

    if (letterCount >= min && letterCount <= max) acc + 1 else acc
  }
  println(result)

/**
 * Given the same schema <number>-<number> <letter>: <password> this time interpreted as
 * <firstIdx>-<secondIdx> <letter>: <password> verify if exacly one char at positions <firstIdx> or <secondIdx> is <letter>
 * Indices are 1-based (Hello Fortran not my old friend)
 * 2-4 p: vpkpp
 * 2-10 b: bfbbbbcbnpbbbbt
 */
@main
def day2Task2() =
  val lines = Source.fromResource("day2.txt").getLines
  val result = lines.foldLeft(0) { (acc, line) =>
    val (first, second, letter, password) = parseLine(line)
    val isValid = (password(first - 1) == letter, password(second - 1) == letter) match {
      case (true, false) | (false, true) => true
      case _ => false
    }

    if (isValid) acc + 1 else acc
  }
  println(result)
