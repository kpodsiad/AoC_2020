import scala.io.Source

object Day4 extends App {
  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val lines = (Source.fromResource("day4.txt").getLines() ++ List("")).foldLeft((0, new StringBuilder())) { (a, line) =>
    val (acc, stringBuilder) = a
    if (line.length != 0) {
      stringBuilder.append(line)
      stringBuilder.append(' ')
      (acc, stringBuilder)
    } else {
      stringBuilder.dropRight(1)
      val x = stringBuilder.result().split(' ').map(_.takeWhile(_ != ':')).toSet
      val newAcc = if (required subsetOf x) acc+1 else acc
      (newAcc, stringBuilder.empty)
    }
  }
  println(lines)
}
