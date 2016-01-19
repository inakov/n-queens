package queens

import scala.util.Random

/**
 * Created by inakov on 16-1-19.
 */
object nQueens extends App{

//  show()

  def nqueens(n: Int): List[Int] = {


//    def min_conflicts(soln, nr, iters=1000):
//    def random_pos(li, filt):
//    return random.choice([i for i in range(nr) if filt(li[i])])
//
//    for k in range(iters):
//      confs = find_conflicts(soln, nr)
//    if sum(confs) == 0:
//    return soln
//    col = random_pos(confs, lambda elt: elt > 0)
//    vconfs = [hits(soln, nr, col, row) for row in range(nr)]
//    soln[col] = random_pos(vconfs, lambda elt: elt == min(vconfs))
//    raise Exception("Incomplete solution: try more iterations.")

    def minConflicts(solution: List[Int]): List[Int] ={
      val conflicts = findConflicts(solution)
      if(conflicts.sum == 0) solution
      else {
        val col = randomPosition(conflicts)
        val rowMap = solution.length
        val queensWithRow = (0 until rowMap) zip solution
        queensWithRow.map(queenWithRow => hits(queenWithRow._2, queenWithRow._1, solution)).toList
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

  Nil
  }

  def show(solutions: List[Int]): Unit = {
    println(
      (for { col <- solutions } yield {
        Vector.fill(solutions.length)("_").updated(col, "*").mkString(" ")
      }).mkString("\n")
    )
  }

}
