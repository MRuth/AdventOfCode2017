import scala.io.Source

object Day4 extends App {
  def part1: Unit = {
    val src = Source.fromResource("Day4.in")
    val in = src.getLines().map(_.split("\\s+"))
    val correct = in.count(passphrase =>
      passphrase.distinct.deep == passphrase.deep
    )

    println(s"Part 1: $correct")
  }

  def part2: Unit = {
    val src = Source.fromResource("Day4.in")
    val in = src.getLines().map(_.split("\\s+"))
    val correct = in.count{passphrase =>
      val sorted = passphrase.map(_.sorted)
      sorted.distinct.deep == sorted.deep
    }

    println(s"Part 2: $correct")
  }

  part1
  part2
}