package aliachawaf

import scala.annotation.tailrec

object Diff {

  def diff(repoPath: String) = {

  }

  def getMatrixOfComparison(newFile: List[String], oldFile: List[String]): Map[(Int, Int), Int] = {

    @scala.annotation.tailrec
    def loop(newLine: Int, oldLine: Int, matrix: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {

      if (newLine == newFile.length + 1)
        matrix

      else if (oldLine == oldFile.length + 1)
        loop(newLine + 1, 0, matrix)

      else if (oldLine == 0 || newLine == 0)
        loop(newLine, oldLine + 1, matrix + ((newLine, oldLine) -> 0))

      else {

        val newContent = newFile(newLine - 1)
        val oldContent = oldFile(oldLine - 1)

        if (newContent == oldContent) {
          val value = matrix((newLine - 1, oldLine - 1)) + 1
          loop(newLine, oldLine + 1, matrix + ((newLine, oldLine) -> value))
        }
        else {
          val value = Integer.max(matrix((newLine - 1, oldLine)), matrix((newLine, oldLine - 1)))
          loop(newLine, oldLine + 1, matrix + ((newLine, oldLine) -> value))
        }
      }
    }

    loop(0, 0, Map())
  }

  def getDiffLines(matrix: Map[(Int, Int), Int], sizeNewFile: Int, sizeOldFile: Int): List[(String, Int)] = {

    @tailrec
    def loop(newLine: Int, oldLine: Int, listDiff: List[(String, Int)]): List[(String, Int)] = {

      // All matrix treated
      if (newLine == 0 && oldLine == 0)
        listDiff

      // If we are on the first line, we go "left" till the end
      else if (newLine == 0)
        loop(newLine, oldLine - 1, ("-", oldLine) :: listDiff)

      // If we are on the first column, we go "up" till the end
      else if (oldLine == 0)
        loop(newLine - 1, oldLine, ("+", newLine) :: listDiff)

      else {
        // We compare the element on the top and the element of the left
        if (matrix((newLine, oldLine - 1)) == matrix((newLine - 1, oldLine))) {
          // if they are equal, we check the element on top left diagonal
          if ((matrix(newLine, oldLine) - 1) == matrix(newLine - 1, oldLine - 1))
            loop(newLine - 1, oldLine - 1, listDiff)
          else
            loop(newLine, oldLine - 1, ("-", oldLine) :: listDiff)
        }

        else if (matrix((newLine - 1, oldLine)) > matrix((newLine, oldLine - 1)))
          loop(newLine - 1, oldLine, ("+", newLine) :: listDiff)

        else
          loop(newLine, oldLine - 1, ("-", oldLine) :: listDiff)
      }
    }

    loop(sizeNewFile, sizeOldFile, List())
  }

}
