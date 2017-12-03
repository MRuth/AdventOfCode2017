import scala.io.Source

object Day1 extends App {
  def part1: Unit = {
    val src = Source.fromResource("Day1.in")
    val in = src.getLines().takeWhile(_ != "").flatMap(_.split("").map(_.toInt)).toList
    val result = (in :+ in.head).sliding(2).filter{case x::y::Nil => x == y}.map(_.head).sum
    println(s"Part 1: $result")
  }

  def part2: Unit = {
    val src = Source.fromResource("Day1.in")
    val in = src.getLines().takeWhile(_ != "").flatMap(_.split("").map(_.toInt)).toArray

    val result = in.zipWithIndex.filter{case (x,i) => x == in((i+(in.length/2)) % in.length)}.map(_._1).sum

    println(s"Part 2: $result")
  }

  part1
  part2
}
