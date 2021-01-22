import scala.annotation.tailrec
import scala.io.Source
import scala.quoted.Consts
import scala.{Right, util}

def findTwoNumbersThatSumUpTo(numbers: IterableOnce[Int], sumUpTo: Int): Option[Int] =
  LazyList.from(numbers).scanLeft[Either[Set[Int], Int]](Left(Set.empty)) {
    case (Left(set), n) =>
      val complement = sumUpTo - n
      if (set(complement)) Right(n * complement) else Left(set + n)
    case (Right(value), _) =>
      Right(value)
  }.collectFirst {
    case x: Right[Set[Int], Int] => x.value
  }

/**
 * Find the two entries that sum to 2020 and multiply those two numbers together
 */
@main
def fun1() =
  val lines = Source.fromResource("day1.txt").getLines.map(_.toInt)
  findTwoNumbersThatSumUpTo(lines, 2020).foreach(println)

/**
 * Find the three entries that sum to 2020 and multiply those two numbers together
 * 
 */
@main
def fun2() =
  val lines = Source.fromResource("day1.txt").getLines.map(_.toInt).toVector
  findTwoNumbersThatSumUpTo(lines, 2020)
  val mappedList: LazyList[(Int, Option[Int])] = LazyList
    .from(lines)
    // break searching for 3 numbers to smaller problem which is lookinjg for 2 numbers
    .map(n => (n, findTwoNumbersThatSumUpTo(lines, 2020 - n)))
  
  mappedList.collectFirst {
    case (i, Some(value)) => i * value
  }.foreach(println)
  