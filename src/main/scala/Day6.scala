import scala.io.Source

object Day6 extends App {
  val src = Source.fromResource("Day6.in").getLines().next
  val starting = src.split("\t").map(_.toInt).toVector
  val stream = Stream.iterate(starting){ banks =>
    val (maxBlocks,maxIdx) = banks.zipWithIndex.maxBy{case (blocks,idx) => (blocks,-idx)}
    val div = maxBlocks/banks.length
    val rem = maxBlocks%banks.length

    val nBanks = (1 to maxBlocks).foldLeft(banks.updated(maxIdx,0)){case (banks,offIdx) =>
      banks.updated((maxIdx+offIdx)%banks.length,banks((maxIdx+offIdx)%banks.length)+1)
    }

  nBanks
  }

  def findFirstRepeat[A](s: Stream[A], seen: Set[A] = Set.empty[A]): Option[Int] = s match {
    case head #:: tail if seen(head) => Some(seen.size)
    case head #:: tail => findFirstRepeat(tail,seen+head)
    case _             => None
  }

  lazy val part1Result = findFirstRepeat(stream)
  def part1: Unit = {
    println(s"Part 1: ${part1Result.get}")
  }

  def part2: Unit = {
    val result = part1Result.get - stream.indexOf(stream(part1Result.get))
   println(s"Part 2: $result")
  }

  part1
  part2
}