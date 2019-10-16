package aliachawaf

import java.io.File

import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil}
import aliachawaf.util.ResultUtil.logNotCommit

object Log {

  def log(repoPath: String): String = {

    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    if (lastCommit.isEmpty) logNotCommit()

    else {
      val allCommits = getCommitLog(repoPath, lastCommit.get)
      logResult(allCommits)
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

      val currentCommitContent = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + currentCommitHash)
      val resultUpdated = (currentCommitHash, currentCommitContent mkString "\n") :: result

      if (currentCommitContent(1).split(" ")(0) == "parent") {

        val parentHash = currentCommitContent(1).split(" ")(1)
        loop(parentHash, resultUpdated)
      }
      else resultUpdated
    }

    loop(lastCommit, List())
  }

  def logResult(commits: List[(String, String)]): String = {

    @scala.annotation.tailrec
    def loop(currentCommits: List[(String, String)], result: String): String = {

      currentCommits match {
        case Nil => result
        case head :: tail =>
          val message = CommitUtil.getCommitMessage(head._2.split("\n").toList)
          val resultUpdated = "commit " + head._1 + "\n     " + message + "\n\n" + result
          loop(tail, resultUpdated)
      }
    }
    loop(commits, "")
  }
}
