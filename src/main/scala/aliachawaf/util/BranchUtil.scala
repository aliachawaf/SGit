package aliachawaf.util

import java.io.File

object BranchUtil {

  def getCurrentBranch(repoPath: String): String = {
    FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "HEAD") mkString "\n"
  }

  def getCurrentBranchName(repoPath: String): String = {
    getCurrentBranch(repoPath).split(File.separator).last
  }

  /**
   *
   * @param repoPath
   * @return List of branches with the format (branchName, commitHash, commitMessage)
   */
  def getAllBranches(repoPath: String) : List[(String, String, String)] = {

    val branchPath = repoPath + File.separator + ".sgit" + File.separator + "branches"

    @scala.annotation.tailrec
    def loop(listBranches: List[File], accBranchList: List[(String, String, String)]) : List[(String, String, String)] = {

      listBranches match {
        case Nil => accBranchList
        case head :: tail => {

          val branchCommit = FileUtil.getFileContent(branchPath + File.separator + head.getName)(0)
          val commitContent = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + branchCommit)
          val commitMsg = CommitUtil.getCommitMessage(commitContent)

          loop(tail, (head.getName, branchCommit, commitMsg) :: accBranchList)
        }
      }
    }
    val branches = new File(branchPath).listFiles().toList
    loop(branches, List())
  }

}
