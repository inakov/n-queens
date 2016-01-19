package queens

import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by inakov on 16-1-19.
 */
object nQueens extends App{

  show(nqueens(4))

  def nqueens(n: Int): List[Int] = {

    @tailrec
    def minConflicts(solution: List[Int]): List[Int] ={
      val conflicts = findConflicts(solution)
      if(conflicts.sum == 0) solution
      else {
        val row = randomPosition(conflicts)
        val vconficts =  for(col <- solution.indices) yield hits(col, row, solution)
        minConflicts(solution.updated(row, vconficts.min))
      }
    }

    def randomPosition(conflicts: List[Int]): Int ={
      val rowMap = conflicts.length
      val conflictWithRow = (0 until rowMap) zip conflicts

      Random.shuffle(conflictWithRow.filter(c => c._2>0)).head._1
    }

    def hits(col: Int, row: Int, queens: List[Int]): Int = {
      val rowMap = queens.length
      val queensWithRow = (0 until rowMap) zip queens
      queensWithRow.foldLeft(0){(count, queenWithRow) =>
        val (r, c) = queenWithRow
        if(col != c && math.abs(col - c) != row - r) count
        else count + 1
      }
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
