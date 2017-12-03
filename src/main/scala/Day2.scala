import scala.io.Source

object Day2 extends App {

  def part1: Unit = {
    val src = Source.fromResource("Day2.in")
    val in = src.getLines().map(_.split("\\t").map(_.toInt)).toArray
    val result = in.foldLeft(0){(sum,row) => sum+(row.max - row.min)}
    println(s"Part 1: $result")
  }

  def part2: Unit ={
    val src = Source.fromResource("Day2.in")
    val in = src.getLines().map(_.split("\\t").map(_.toInt)).toArray
    val result = in.foldLeft(0){(sum,row) =>
      val (x,y) = row.combinations(2).collectFirst{
        case Array(a,b) if (a%b == 0) => (a,b)
        case Array(a,b) if (b%a == 0) => (b,a)
      }.get
      sum + (x/y)
    }

    println(s"Part 2: $result")
  }

  part1
  part2
}
