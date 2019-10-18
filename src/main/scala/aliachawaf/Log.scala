package aliachawaf

import java.io.File

import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, ObjectUtil}
import aliachawaf.util.ResultUtil.logNotCommit

object Log {

  def log(repoPath: String, option: String): String = {

    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    if (lastCommit.isEmpty) logNotCommit()
    else {
      val allCommits = getCommitLog(repoPath, lastCommit.get)

      option match {
        case "" => logResult(allCommits, currentBranch)
        case "patch" => getLogOption(allCommits, repoPath, Diff.getDiffResultAllFiles)
        case "stat" => getLogOption(allCommits, repoPath, Diff.getDiffAllFilesOptionStat)
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

  def getLogOption(commits: List[(String, String)], repoPath: String, option: (List[FilesToDiff], String) => String): String = {

    @scala.annotation.tailrec
    def loop(currentCommits: List[(String, String)], result: String): String = {

      currentCommits match {
        case Nil => result
        case head :: tail =>

          val message = CommitUtil.getCommitMessage(head._2.split("\n").toList)
          val parent = CommitUtil.getCommitParent(head._2.split("\n").toList)

          val treeHash = head._2.split("\n").toList(0).split(" ")(1)
          val treeContent = ObjectUtil.getObjectContent(repoPath, treeHash)
          val newFiles = CommitUtil.getCommitAsMap(repoPath, treeContent)

          if (parent.isDefined) {

            val parentContent = ObjectUtil.getObjectContent(repoPath, parent.get)

            val parentTreeContent = ObjectUtil.getObjectContent(repoPath, parentContent(0).split(" ")(1))
            val oldFiles = CommitUtil.getCommitAsMap(repoPath, parentTreeContent)

            // val tuples =
            // getListTuple(newFiles, oldFiles)
            // .map(tuple => (repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + tuple._1, repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + tuple._2, tuple._3))

            val tuples =
              getListTuple(newFiles, oldFiles)
                .map(tuple => new FilesToDiff(ObjectUtil.getObjectContent(repoPath, tuple._1), ObjectUtil.getObjectContent(repoPath, tuple._2), tuple._3))

            val diffWithParent = option(tuples, repoPath)

            val resultUpdated = Console.YELLOW + "commit " + head._1 + Console.RESET + "\n     " + message + "\n" + diffWithParent + "\n\n" + result
            loop(tail, resultUpdated)
          }
          else {

            val tuples =
              getListTuple(newFiles, Map().withDefaultValue(""))
                .map(tuple => new FilesToDiff(ObjectUtil.getObjectContent(repoPath, tuple._1), ObjectUtil.getObjectContent(repoPath, tuple._2), tuple._3))

            val diffWithParent = option(tuples, repoPath)

            val resultUpdated = Console.YELLOW + "commit " + head._1 + Console.RESET + "\n     " + message + "\n" + diffWithParent + "\n\n" + result
            loop(tail, resultUpdated)
          }


      }
    }

    loop(commits, "")
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
