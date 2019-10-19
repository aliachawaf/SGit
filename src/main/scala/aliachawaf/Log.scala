package aliachawaf

import java.io.File

import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, ObjectUtil}
import aliachawaf.util.ResultUtil.logNotCommit

import Console.{RESET, YELLOW}
import scala.annotation.tailrec

class CommitToDiff(val filesToDiff: List[FilesToDiff])

object Log {

  def log(repoPath: String, option: String): String = {

    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    if (lastCommit.isEmpty) logNotCommit()
    else {
      val allCommits = getCommitLog(repoPath, lastCommit.get)
      val allCommitsToDiff = getFilesToDiff_ForAllCommits(allCommits, repoPath)

      option match {
        case "" => logResult(allCommitsToDiff, None, repoPath)
        case "patch" => logResult(allCommitsToDiff, Some(Diff.getDiffResultAllFiles), repoPath)
        case "stat" => logResult(allCommitsToDiff, Some(Diff.getDiffAllFilesOptionStat), repoPath)
      }
    }
  }

  /**
   *
   * @param repoPath
   * @param lastCommit
   * @return
   */
  def getCommitLog(repoPath: String, lastCommit: String): List[(String, String)] = {

    @scala.annotation.tailrec
    def loop(currentCommitHash: String, result: List[(String, String)]): List[(String, String)] = {

      val currentCommitContent = ObjectUtil.getObjectContent(repoPath, currentCommitHash)
      val resultUpdated = (currentCommitHash, currentCommitContent mkString "\n") :: result

      if (currentCommitContent(1).split(" ")(0) == "parent") {

        val parentHash = currentCommitContent(1).split(" ")(1)
        loop(parentHash, resultUpdated)
      }
      else resultUpdated
    }

    loop(lastCommit, List())
  }

  def logResult(commits: List[CommitToDiff], option: Option[(List[FilesToDiff], String) => String], repoPath: String): String = {

    @tailrec
    def loop(remainingCommits: List[CommitToDiff], result: String): String = {

      remainingCommits match {

        case Nil => result

        case commitToDiff :: tail =>

          if (option.isEmpty) {
            val resultUpdated = YELLOW + "commit " + "TO DO GET SHA" + RESET + "\n     " + "TO DO GET MSG" + "\n\n" + result
            loop(tail, resultUpdated)
          }
          else {
            val diffWithParent = option.get(commitToDiff.filesToDiff, repoPath)
            val resultUpdated = YELLOW + "commit " + "TO DO GET SHA" + RESET + "\n     " + "TO DO GET MSG" + "\n" + diffWithParent + "\n\n" + result
            loop(tail, resultUpdated)
          }
      }
    }
    loop(commits, "")
  }


  /**
   *
   * @param newFiles : list of the new version of files, given as Map of (path -> hash)
   * @param oldFiles : list of the old version of files, given as Map of (path -> hash)
   * @param repoPath : path of the sgit repo
   * @return the list of files we want to diff
   */
  def getFilesToDiff_ForOneCommit(newFiles: Map[String, List[String]], oldFiles: Map[String, List[String]], repoPath: String): List[FilesToDiff] = {

    @scala.annotation.tailrec
    def loop(currentNewFiles: Map[String, List[String]], list: List[FilesToDiff]): List[FilesToDiff] = {

      if (currentNewFiles.isEmpty) {

        val deletedFiles = oldFiles.keys.toList diff newFiles.keys.toList
        val deletedTuple = deletedFiles.map(file => new FilesToDiff(List(), oldFiles(file), file))

        deletedTuple ++ list
      }
      else {
        val newFileContent = currentNewFiles.head._2
        val oldFileContent = oldFiles(currentNewFiles.head._1)

        val fileToDiff = new FilesToDiff(newFileContent, oldFileContent, currentNewFiles.head._1)

        loop(currentNewFiles.tail, fileToDiff :: list)
      }
    }

    loop(newFiles, List())
  }


  def getFilesToDiff_ForAllCommits(commits: List[(String, String)], repoPath: String): List[CommitToDiff] = {

    @scala.annotation.tailrec
    def loop(currentCommits: List[(String, String)], result: List[CommitToDiff]): List[CommitToDiff] = {

      currentCommits match {
        case Nil => result

        case head :: tail =>

          val parent = CommitUtil.getCommitParent(head._2.split("\n").toList)

          val treeHash = head._2.split("\n").toList(0).split(" ")(1)
          val newFiles = CommitUtil.getCommitAsMap(repoPath, Some(treeHash))

          if (parent.isDefined) {

            val parentContent = ObjectUtil.getObjectContent(repoPath, parent.get)

            val parentTreeContent = ObjectUtil.getObjectContent(repoPath, parentContent(0).split(" ")(1))
            val parentHash = ObjectUtil.hash(parentTreeContent mkString "\n")
            val oldFiles = CommitUtil.getCommitAsMap(repoPath, Some(parentHash))

            val filesToDiff = getFilesToDiff_ForOneCommit(newFiles.get, oldFiles.get, repoPath)

            val resultUpdated = new CommitToDiff(filesToDiff) :: result
            loop(tail, resultUpdated)
          }
          else {

            val filesToDiff = getFilesToDiff_ForOneCommit(newFiles.get, Map().withDefaultValue(List()), repoPath)

            val resultUpdated = new CommitToDiff(filesToDiff) :: result
            loop(tail, resultUpdated)
          }


      }
    }

    loop(commits, List())
  }
}
