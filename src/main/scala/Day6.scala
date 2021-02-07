import scala.annotation.tailrec
import scala.io.Source

@tailrec
def loop(lines: List[String], answers: Set[Char], acc: Int)(implicit monoid: Monoid[Set[Char]]): Int = {
  lines match {
    case head :: tail if head.nonEmpty =>
      loop(tail, monoid.combine(answers, head.toSet), acc)
    case head :: tail =>
      loop(tail, monoid.empty, acc + answers.size)
    case Nil =>
      acc
  }
}

@main
def day6Task1() =
  val lines = Source.fromResource("day6.txt").getLines().toList ++ List("")
  implicit val monoid: Monoid[Set[Char]] = new Monoid[Set[Char]] {
    override val empty: Set[Char] = Set.empty
    override def combine(x: Set[Char], y: Set[Char]): Set[Char] = x ++ y
  }
  
  val result = loop(lines, monoid.empty, 0)
  println(result)

@main
def day6Task2() =
  val lines = Source.fromResource("day6.txt").getLines().toList ++ List("")
  implicit val monoid: Monoid[Set[Char]] = new Monoid[Set[Char]] {
    override val empty: Set[Char] = ('a' to 'z').toSet
    override def combine(x: Set[Char], y: Set[Char]): Set[Char] = x intersect y
  }
  
  val result = loop(lines, monoid.empty, 0)
  println(result)
