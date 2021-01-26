
import scala.io.Source

val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
def parseInput(lines: IterableOnce[String], validatorFn: String => String => Boolean) =
  lines.iterator.foldLeft((0, Map.empty[String, String])) { (acc, line) =>
    val (valid, fields) = acc
    if (line.length != 0) {
      val newFields = line.split(' ')
        .map { pair =>
          val arr = pair.split(':')
          (arr(0), arr(1))
        }.toMap
      (valid, fields ++ newFields)
    } else {
      val areFieldsValid = fields.map((k: String, v: String) => validatorFn(k)(v)).forall(_ == true)
      val keys = fields.keys.toSet
      val newValid = if (areFieldsValid && required.subsetOf(keys)) valid + 1 else valid
      (newValid, Map.empty)
    }
  }

@main
def day4Task1() =
  val lines = (Source.fromResource("day4.txt").getLines() ++ List(""))
  val (result, _) = parseInput(lines, _ => _ => true)
  println(result)

@main
def day4Task2() =
  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val validators: Map[String, String => Boolean] = Map(
    "byr" -> """19[2-9]\d|200[0-2]""".r.matches,
    "iyr" -> """20[1-2]\d""".r.matches,
    "eyr" -> """20[2-3]\d""".r.matches,
    "hgt" -> """(1[5-8]\d|19[1-3])cm|(59|6\d|7[0-6])in""".r.matches,
    "hcl" -> """#[0-9a-f]{6}""".r.matches,
    "ecl" -> """amb|blu|brn|gry|grn|hzl|oth""".r.matches,
    "pid" -> """\d{9}""".r.matches,
    "cid" -> (_.length > 0)
  )
  val lines = (Source.fromResource("day4.txt").getLines() ++ List(""))
  val (result, _) = parseInput(lines, validators(_))

  println(result)
