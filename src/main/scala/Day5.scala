import scala.io.Source

object Day5 extends App {
  def processMaze(maze: Vector[Int], incFunc: Int => Int): Int = {
    def recur(maze: Vector[Int], i: Int, cStep: Int): Int = {
      maze.lift(i) match {
        case Some(offset) =>
          recur(maze.updated(i,offset + incFunc(offset)),i+offset,cStep+1)
        case _ => cStep
      }
    }
    recur(maze, 0, 0)
  }

  val src = Source.fromResource("Day5.in").getLines()
  val in = src.map(_.toInt).toVector

  def part1: Unit = {
    val result = processMaze(in,_ => 1)
    println(s"Part 1: $result")
  }

  def part2: Unit = {
    val result = processMaze(in, x => if(x >= 3) -1 else 1)
    println(s"Part 2: $result")
  }

  part1
  part2
}