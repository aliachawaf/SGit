package aliachawaf

import java.io.File
import java.io.File.separator

import aliachawaf.util.ResultUtil._
import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil}

object Tag {

  def tag(repoPath: String, name: String): String = {

    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    // Check if there is a commit, else no tag can be created
    if (lastCommit.isDefined) {

      val tagPath = repoPath + separator + ".sgit" + separator + "tags" + separator + name

      if (new File(tagPath).exists()) tagResult(created = false, name)
      else {
        FileUtil.createNewFile(tagPath, lastCommit.get)
        tagResult(created = true, name)
      }
    }
    else tagNoCommit()
  }
}
