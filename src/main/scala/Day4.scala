import scala.io.Source

object Day4 extends App {
  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val lines = (Source.fromResource("day4.txt").getLines() ++ List(""))
  val (result, _) = lines.foldLeft((0, Set.empty[String])) { (acc, line) =>
    val (valid, fields) = acc
    if (line.length != 0) {
      val newFields = line.split(' ').map(_.takeWhile(_ != ':')).toSet
      (valid, fields ++ newFields)
    } else {
      val newValid = if (required subsetOf fields) valid+1 else valid
      (newValid, Set.empty)
    }
  }
  println(result)
}
