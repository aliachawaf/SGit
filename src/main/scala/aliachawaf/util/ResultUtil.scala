package aliachawaf.util

import java.io.File
import Console.{GREEN, RED, RESET}

import aliachawaf.Status.{get_Tracked_Committed_Modified, get_Tracked_Modified_NotAdded, get_Tracked_NeverCommitted, get_Untracked}

object ResultUtil {

  def notSGitRepository(): String = "fatal: not a sgit repository (or any of the parent directories): .sgit"

  def addResult(nbFiles: Int) = nbFiles + " file(s) added to index."

  def nothingToCommit(repoPath: String) = "On branch " + BranchUtil.getCurrentBranchName(repoPath) + "/n/n nothing to commit"

  def initResult(result: Boolean, currentDir: String): String = {
    if (result) "Initialized empty Sgit repository in " + currentDir + File.separator + ".sgit" + File.separator
    else "Already initialized SGit repository"
  }

  // TODO Add files deletions and additions
  def commitResult(branch: String, hash: String, message: String): String = "[" + branch + " " + hash.slice(0, 7) + "] " + message + "\n "

  def sameCommitResult(branch: String): String = "On branch " + branch + "\nNothing to commit, working tree clean."

  def statusResult(branch: String,
                   tracked_modified_notAdded: List[String],
                   deleted_notAdded: List[String],
                   tracked_committed_modified: List[String],
                   tracked_neverCommitted: List[String],
                   deleted_notCommitted: List[String],
                   untracked: List[String]): String = {

    "On branch " + branch + ".\n\n" +
    "Changes to be committed:\n\n" +
      GREEN + "new file: " + (tracked_modified_notAdded.mkString("\nnew file: ")) +
      "modified: " + (tracked_committed_modified.mkString("\nmodified: ")) +
      "deleted: " + (deleted_notCommitted.mkString("\ndeleted: ")) +
      RESET + "\n\n" +
      "Changes not staged for commit:\n (use \"sgit add<file>...\" to update what will be committed)\n\n" +
      RED + (tracked_neverCommitted mkString "\n") +
      "deleted: " + (deleted_notAdded.mkString("\ndeleted: ")) +
      RESET + "\n\n" +
      "Untracked files:\n (use \"sgit add <file>...\" to include in what will be committed)\n\n" +
      RED + (untracked mkString "\n") +
      RESET + "\n\n"
  }

  def statusNoCommit(repoPath: String): String = "On branch " + BranchUtil.getCurrentBranchName(repoPath) + "\n\nNo commits yet."

  /** TAG **/
  def tagResult(created: Boolean, name: String) = {
    if (created) "Tag '" + name + "' created"
    else "fatal: tag '" + name + "' already exists"
  }

  def tagNoCommit() = "fatal: Failed to resolve 'HEAD' as a valid ref (i.e. there is no commit to tag)."

  /** TAG **/
  def branchResult(created: Boolean, name: String) = {
    if (created) "Branch '" + name + "' created"
    else "fatal: branch named '" + name + "' already exists"
  }

  def branchNoMaster() = "fatal: Not a valid object name: 'master'."

  def branchAVResult(branches: List[BranchTag], tags: List[BranchTag], currentBranch: String) : String = {

    val current = branches.filter(_.name == currentBranch).head
    val resultCurrentBranch = "branches : \n" + Console.GREEN + current.name + " " + current.hash.slice(0,7) + " " + current.commitMessage + Console.RESET + "\n"
    val resultBranches = resultCurrentBranch + branches.filter(_.name != currentBranch).map(b => b.toString).mkString("\n")

    resultBranches + "\n\ntags : \n" + tags.map(t => t.toString).mkString("\n")
  }

  def logNotCommit() = "There is no commit yet"

}
