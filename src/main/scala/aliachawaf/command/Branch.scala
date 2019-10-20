package aliachawaf.command

import java.io.File

import aliachawaf.util.ResultUtil.{branchNoMaster, branchResult}
import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, ResultUtil}

object Branch {

  /**
   *
   * @param repoPath : path of the SGit repository
   * @param option   : true for "branch -av", false for creating a branch
   * @param name     : name of the new branch (empty if option is true)
   * @return branch command result
   */
  def branch(repoPath: String, option: Boolean, name: String): String = {

    if (option) branchAV(repoPath)
    else createNewBranch(repoPath, name)
  }

  /**
   * Create a new branch for the given SGit repository
   *
   * @param repoPath : path of the sgit repository
   * @param name     : name of the new branch to create
   * @return the result of branch creating to print (success, fail ..)
   */
  def createNewBranch(repoPath: String, name: String): String = {

    // Cannot create a branch if master branch is not created yet (no commit)
    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = CommitUtil.getLastCommit(repoPath, currentBranch)

    if (lastCommit.isDefined) {

      val branchPath = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + name

      if (new File(branchPath).exists()) branchResult(created = false, name)
      else {
        FileUtil.createNewFile(branchPath, lastCommit.get)
        branchResult(created = true, name)
      }
    }
    else branchNoMaster()
  }


  /**
   * List all the branches and tags of the given SGit repository
   *
   * @param repoPath : path of the SGit repository
   * @return branch -av result
   */
  def branchAV(repoPath: String): String = {

    val branches = BranchUtil.getAllBranchesOrTags("branches", repoPath)
    val tags = BranchUtil.getAllBranchesOrTags("tags", repoPath)
    val currentBranch = BranchUtil.getCurrentBranchName(repoPath)

    ResultUtil.branchAVResult(branches, tags, currentBranch)
  }
}
