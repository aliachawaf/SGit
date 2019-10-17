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
      logResult(allCommits, currentBranch)
    }
  }

  def logP(repoPath: String): String = {

    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    if (lastCommit.isEmpty) logNotCommit()
    else {
      val allCommits = getCommitLog(repoPath, lastCommit.get)
      logOptionP(allCommits, currentBranch, repoPath)
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

  def logResult(commits: List[(String, String)], branchName: String): String = {

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

    "On branch " + branchName + "\n\n" + loop(commits, "")
  }


  def logOptionP(commits: List[(String, String)], branchName: String, repoPath: String): String = {

    @scala.annotation.tailrec
    def loop(currentCommits: List[(String, String)], result: String): String = {

      currentCommits match {
        case Nil => result
        case head :: tail =>

          val message = CommitUtil.getCommitMessage(head._2.split("\n").toList)
          val parent = CommitUtil.getCommitParent(head._2.split("\n").toList)

          val treeHash = head._2.split("\n").toList(0).split(" ")(1)
          val treeContent = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + treeHash)
          val newFiles = CommitUtil.getCommitAsMap(repoPath, treeContent)

          if (parent.isDefined) {

            val parentContent = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + parent.get)

            val parentTreeContent = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + parentContent(0).split(" ")(1))
            val oldFiles = CommitUtil.getCommitAsMap(repoPath, parentTreeContent)

            val tuples =
              getListTuple(newFiles, oldFiles)
                .map(tuple => (repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + tuple._1, repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + tuple._2, tuple._3))

            val diffWithParent = Diff.getDiffResultAllFiles(tuples, repoPath)

            val resultUpdated = "commit " + head._1 + "\n     " + message + "\n" + diffWithParent + "\n\n" + result
            loop(tail, resultUpdated)

          }
          else {

            val tuples =
              getListTuple(newFiles, Map().withDefaultValue(""))
                .map(tuple => (repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + tuple._1, repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + tuple._2, tuple._3))

            val diffWithParent = Diff.getDiffResultAllFiles(tuples, repoPath)

            val resultUpdated = "commit " + head._1 + "\n     " + message + "\n" + diffWithParent + "\n\n" + result
            loop(tail, resultUpdated)
          }


      }
    }

    "On branch " + branchName + "\n\n" + loop(commits, "")
  }

  def getListTuple(newFiles: Map[String, String], oldFiles: Map[String, String]): List[(String, String, String)] = {

    @scala.annotation.tailrec
    def loop(currentNewFiles: Map[String, String], list: List[(String, String, String)]): List[(String, String, String)] = {

      if (currentNewFiles.isEmpty) {

        val deletedFiles = oldFiles.keys.toList diff newFiles.keys.toList
        val deletedTuple = deletedFiles.map(file => ("", oldFiles(file), file))

        deletedTuple ++ list
      }
      else {
        val newFile = currentNewFiles.head._2
        val oldFile = oldFiles(currentNewFiles.head._1)

        loop(currentNewFiles.tail, (newFile, oldFile, currentNewFiles.head._1) :: list)
      }
    }

    loop(newFiles, List())
  }
}
