

object Day3 extends App {
  def getIter = Iterator.iterate((0,0,0,1,true))
  { case (x, y, cStep, stps, mvX) =>
    (
      if (mvX && stps % 2 == 1)
        x + 1
      else if (mvX && stps % 2 == 0)
        x - 1
      else x,
      if (!mvX && stps % 2 == 1)
        y + 1
      else if (!mvX && stps % 2 == 0)
        y - 1
      else y,
      (cStep + 1) % stps,
      if (!mvX && (cStep + 1) % stps == 0) stps + 1 else stps,
      if ((cStep + 1) % stps == 0) !mvX else mvX
    )
  }

  def part1: Unit = {
    val in = 277678
    val itr = getIter
    val (x,y,_,_,_) = itr.drop(in-1).next()
    val manhattan = (0-x).abs+(0-y).abs
    println(s"Part 1: $manhattan")
  }

  def part2: Unit = {
    val in = 277678
    val sums = collection.mutable.HashMap.empty[(Int,Int),Int].+=((0,0) -> 1)
    val adjacents = List((1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1))
    def getSumOfAdjacents(x: Int,y:Int) = adjacents.flatMap{case (dx,dy) => sums.get(x+dx,y+dy)}.sum

    val itr = getIter
    itr.take(in).map(v => (v._1,v._2)).dropWhile{case (x,y) =>
      val value = sums.getOrElseUpdate((x,y), getSumOfAdjacents(x,y))
      value < in
    }.next
    val result = sums.values.max

    println(s"Part 2: $result")
  }

  part1
  part2
}