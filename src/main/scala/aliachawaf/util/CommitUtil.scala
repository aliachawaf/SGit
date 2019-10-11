package aliachawaf.util

import java.io.File

object CommitUtil {

  def getLastCommit(repoPath: String, currentBranch: String): Option[String] = {
    val branchPath = repoPath + File.separator + ".sgit" + File.separator + currentBranch
    if (new File(branchPath).exists()) Some(FileUtil.getFileContent(branchPath) mkString "\n")
    else None
  }

  def getLastCommitTree(repoPath: String): Option[String] = {
    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = getLastCommit(repoPath, currentBranch)

    if (lastCommit.isDefined) {
      val lastCommitContent = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + lastCommit.get)
      Some(lastCommitContent(0).split(" ")(1))
    }
    else None
  }

  // TODO Add files deletions and additions
  def resultMessage(branch: String, hash: String, message: String): String = "[" + branch + " " + hash.slice(0, 8) + "] " + message + "\n "

  def resultMessageSameCommit(branch: String): String = "On branch " + branch + "\nNothing to commit, working tree clean."

}
