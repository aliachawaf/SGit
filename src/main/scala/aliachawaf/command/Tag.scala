package aliachawaf.command

import java.io.File

import aliachawaf.util.ResultUtil.{tagNoCommit, tagResult}
import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil}

object Tag {

  /**
   * Create a new tag for the given SGit repository
   *
   * @param repoPath : path of the sgit repository
   * @param name     : name of the new tag to create
   * @return the result of tag creating to print (success, fail ..)
   */
  def tag(repoPath: String, name: String): String = {

    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    // Check if there is a commit, else no tag can be created
    if (lastCommit.isDefined) {
      val tagPath = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + name

      if (new File(tagPath).exists()) tagResult(created = false, name)
      else {
        FileUtil.createNewFile(tagPath, lastCommit.get)
        tagResult(created = true, name)
      }
    }
    else tagNoCommit()
  }
}
