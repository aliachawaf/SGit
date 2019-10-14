package aliachawaf

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
          val value = matrix((newLine - 1, oldLine - 1))
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
}
