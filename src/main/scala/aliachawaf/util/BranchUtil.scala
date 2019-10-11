package aliachawaf.util

import java.io.File

object BranchUtil {

  def getCurrentBranch(repoPath: String): String = {
    FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "HEAD") mkString "\n"
  }

  def getCurrentBranchName(repoPath: String): String = {
    getCurrentBranch(repoPath).split(File.separator).last
  }

}
