import scala.io.Source
import scala.quoted.Consts
import scala.{Right, util}

@main
def fun1() =
  type Result = Either[Set[Int], Int]
  val lines = Source.fromResource("day1.txt").getLines.map(_.toInt)
  val sumUpTo = 2020
  lines.to(LazyList).scanLeft[Result](Left(Set.empty)) {
    case (Left(set), n) => 
      val complement = sumUpTo - n
      if (set(complement)) Right(n * complement) else Left(set + n)
    case (Right(value), _) => 
      Right(value)
  }.collectFirst {
    case x: Right[Set[Int], Int] => x.value
  }.foreach(println)
  
