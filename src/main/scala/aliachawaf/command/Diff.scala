package aliachawaf.command

import java.io.File

import aliachawaf.util.{FileUtil, IndexUtil, ObjectUtil}

import scala.Console.{GREEN, RED, RESET}
import scala.annotation.tailrec

class FilesToDiff(val newFileContent: List[String], val oldFileContent: List[String], val filePath: String)

object Diff {

  def diff(repoPath: String): String = {

    /*_______________ I/O PART _______________*/
    val indexMap = IndexUtil.getIndexAsMap(repoPath)
    val listNewOld = getAllFilesToDiff(repoPath, indexMap.toList)

    /*_______________ PURE PART _______________*/
    getDiffResultAllFiles(listNewOld, repoPath)

  }

  /**
   *
   * @param repoPath : path of the SGit repository
   * @param indexMap : index given as a Map with (path -> blobHash)
   * @return all the files we have to do the diff on it
   */
  def getAllFilesToDiff(repoPath: String, indexMap: List[(String, String)]): List[FilesToDiff] = {

    @tailrec
    def loop(remainingIndexPath: List[(String, String)], accFiles: List[FilesToDiff]): List[FilesToDiff] = {

      remainingIndexPath match {
        case Nil => accFiles
        case head :: tail => {

          val newFileContent = FileUtil.getFileContent(repoPath + File.separator + head._1)
          val oldFileContent = ObjectUtil.getObjectContent(repoPath, head._2)
          val filePath = head._1
          val fileToDiff = new FilesToDiff(newFileContent, oldFileContent, filePath)

          loop(tail, fileToDiff :: accFiles)
        }
      }
    }
    loop(indexMap, List())
  }

  /**
   *
   * @param newFile : content of the new file version as (list of the lines)
   * @param oldFile : content of the old file version as (list of the lines)
   * @return the matrix of the longest common subsequence given as Map ( (line index in newFile, line index in oldFile) -> value)
   */
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

  /**
   *
   * @param matrix
   * @param sizeNewFile
   * @param sizeOldFile
   * @return
   */
  def getDiffLines(matrix: Map[(Int, Int), Int], sizeNewFile: Int, sizeOldFile: Int): List[(String, Int)] = {

    @tailrec
    def loop(newLine: Int, oldLine: Int, listDiff: List[(String, Int)]): List[(String, Int)] = {

      // All matrix treated
      if (newLine == 0 && oldLine == 0)
        listDiff

      // If we are on the first line, we go "left" till the end
      else if (newLine == 0)
        loop(newLine, oldLine - 1, ("--", oldLine) :: listDiff)

      // If we are on the first column, we go "up" till the end
      else if (oldLine == 0)
        loop(newLine - 1, oldLine, ("++", newLine) :: listDiff)

      else {
        // We compare the element on the top and the element of the left
        if (matrix((newLine, oldLine - 1)) == matrix((newLine - 1, oldLine))) {
          // if they are equal, we check the element on top left diagonal
          if ((matrix(newLine, oldLine) - 1) == matrix(newLine - 1, oldLine - 1))
            loop(newLine - 1, oldLine - 1, listDiff)
          else
            loop(newLine, oldLine - 1, ("--", oldLine) :: listDiff)
        }

        else if (matrix((newLine - 1, oldLine)) > matrix((newLine, oldLine - 1)))
          loop(newLine - 1, oldLine, ("++", newLine) :: listDiff)

        else
          loop(newLine, oldLine - 1, ("--", oldLine) :: listDiff)
      }
    }

    loop(sizeNewFile, sizeOldFile, List())
  }

  /**
   *
   * @param linesDiff
   * @param newFile
   * @param oldFile
   * @return
   */
  def getDiffResultOneFile(linesDiff: List[(String, Int)], newFile: List[String], oldFile: List[String]): String = {

    @tailrec
    def loop(result: String, currentListDiff: List[(String, Int)]): String = {

      currentListDiff match {
        case Nil => result
        case head :: tail => {
          if (head._1 == "++") {
            val newLine = GREEN + "(line " + head._2 + " " + head._1 + ") " + newFile(head._2 - 1) + RESET
            loop(result + newLine + "\n", tail)
          }
          else {
            val deletedLine = RED + "(line " + head._2 + " " + head._1 + ") " + oldFile(head._2 - 1) + RESET
            loop(result + deletedLine + "\n", tail)
          }
        }
      }
    }

    loop("", linesDiff)
  }

  /**
   *
   * @param listNewOld
   * @param repoPath
   * @return
   */
  def getDiffResultAllFiles(listNewOld: List[FilesToDiff], repoPath: String): String = {

    @tailrec
    def loop(currentListNewOld: List[FilesToDiff], result: String): String = {

      currentListNewOld match {
        case Nil => result
        case headCouple :: tail =>

          // Content of file in working tree
          val newFile = headCouple.newFileContent
          val oldFile = headCouple.oldFileContent

          // Get diff for the file
          val matrix = getMatrixOfComparison(newFile, oldFile)
          val diffLines = getDiffLines(matrix, newFile.length, oldFile.length)

          if (diffLines.isEmpty) loop(tail, result + "\n\n")
          else {
            val diffResult = headCouple.filePath.replace(repoPath + File.separator, "") + " : \n" + getDiffResultOneFile(diffLines, newFile, oldFile) + "\n\n"
            loop(tail, result + diffResult)
          }
      }
    }

    loop(listNewOld, "")
  }


  /**
   *
   * @param listNewOld
   * @param repoPath
   * @return
   */
  def getDiffAllFilesOptionStat(listNewOld: List[FilesToDiff], repoPath: String): String = {
    @tailrec
    def loop(currentListNewOld: List[FilesToDiff],
             result: String,
             accFilesModified: Int,
             accAdditions: Int,
             accDeletions: Int
            ): String = {

      currentListNewOld match {
        case Nil => result + accFilesModified + " file(s) changed, " + accAdditions + " insertions(++), " + accDeletions + " deletions(--)\n"

        case headCouple :: tail =>

          // Content of file in working tree
          val newFile = headCouple.newFileContent
          val oldFile = headCouple.oldFileContent

          // Get diff for the file
          val matrix = getMatrixOfComparison(newFile, oldFile)
          val diffLines = getDiffLines(matrix, newFile.length, oldFile.length)

          if (diffLines.isEmpty) loop(tail, result, accFilesModified, accAdditions, accDeletions)

          else {

            val accAdditionsUpdated = diffLines.count(_._1 == "++")
            val accDeletionsUpdated = diffLines.count(_._1 == "--")
            val totalDiff = accAdditionsUpdated + accDeletionsUpdated

            val fileStat =
              totalDiff + " : " +
                GREEN + accAdditionsUpdated + "++ " + RESET +
                RED + accDeletionsUpdated + "--" + RESET +
                "\n"

            val diffResult = headCouple.filePath.replace(repoPath + File.separator, "") + " | " + fileStat
            loop(tail, result + diffResult, accFilesModified + 1, accAdditionsUpdated, accDeletionsUpdated)
          }
      }
    }

    loop(listNewOld, "", 0, 0, 0)
  }
}
