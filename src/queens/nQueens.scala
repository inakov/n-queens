package queens

import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by inakov on 16-1-19.
 */
object nQueens extends App{

  show(nqueens(8))

  def nqueens(n: Int): List[Int] = {

    @tailrec
    def minConflicts(solution: List[Int]): List[Int] ={
      val conflicts = findConflicts(solution)
      if(conflicts.sum == 0) solution
      else {
        val row = randomPositionInConflict(conflicts)
        val vconficts =  for(col <- solution.indices) yield (col, hits(col, row, solution))
        minConflicts(solution.updated(row, vconficts.minBy(_._2)._1))
      }
    }

    def randomPositionInConflict(conflicts: List[Int]): Int ={
      val rowMap = conflicts.length
      val conflictWithRow = (0 until rowMap) zip conflicts
      val filteredConflict = conflictWithRow.filter(_._2>0)

      val rand = new Random(System.currentTimeMillis())
      val randomIndex = rand.nextInt(filteredConflict.length)

      filteredConflict(randomIndex)._1
    }

    def hits(col: Int, row: Int, queens: List[Int]): Int = {
      val rowMap = queens.length
      val queensWithRow = (0 until rowMap) zip queens
      queensWithRow.foldLeft(0)({(count, queenWithRow) =>
        val (r, c) = queenWithRow
        if(row != r && (col == c || math.abs(col - c) == math.abs(row - r))) count + 1
        else count
      })
    }

    def findConflicts(queens: List[Int]): List[Int] = {
      val rowMap = queens.length
      val queensWithRow = (0 until rowMap) zip queens
      queensWithRow.map(queenWithRow => hits(queenWithRow._2, queenWithRow._1, queens)).toList
    }

    minConflicts(List.range(0, n))
  }

  def show(solutions: List[Int]): Unit = {
    println(
      (for { col <- solutions } yield {
        Vector.fill(solutions.length)("_").updated(col, "*").mkString(" ")
      }).mkString("\n")
    )
  }

}
