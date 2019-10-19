package aliachawaf

import java.io.File
import java.io.File.separator

import aliachawaf.util.{BranchTag, BranchUtil, CommitUtil, FileUtil, ResultUtil}
import aliachawaf.util.ResultUtil.{branchNoMaster, branchResult}


object Branch {

  /**
   *
   * @param repoPath : path of the sgit repository
   * @param name : name of the new branch to create
   * @return the result of branch creating to print (success, fail ..)
   */
  def createNewBranch(repoPath: String, name: String) : String = {

    // Cannot create a branch if master branch is not created yet (no commit)
    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    if (lastCommit.isDefined) {

      val branchPath = repoPath + separator + ".sgit" + separator + "branches" + separator + name

      if (new File(branchPath).exists()) branchResult(created = false, name)
      else {
        FileUtil.createNewFile(branchPath, lastCommit.get)
        branchResult(created = true, name)
      }
    }
    else branchNoMaster()
  }


  def branchAV(repoPath: String) : String = {

    val branches = BranchUtil.getAllBranchesOrTags("branches", repoPath)
    val tags = BranchUtil.getAllBranchesOrTags("tags", repoPath)
    val currentBranch = BranchUtil.getCurrentBranchName(repoPath)

    ResultUtil.branchAVResult(branches, tags, currentBranch)
  }
}
