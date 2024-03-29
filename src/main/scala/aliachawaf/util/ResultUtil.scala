package aliachawaf.util

import java.io.File

import aliachawaf.command.{CommitToDiff, FilesToDiff}

import scala.Console.{GREEN, RED, RESET, YELLOW}
import scala.annotation.tailrec

object ResultUtil {

  /* REPOSITORY */
  def notSGitRepository(): String = "fatal: not a sgit repository (or any of the parent directories): .sgit"

  /* INIT */
  def initResult(result: Boolean, currentDir: String): String = {
    if (result) "Initialized empty Sgit repository in " + currentDir + File.separator + ".sgit" + File.separator
    else "Already initialized SGit repository"
  }

  /* ADD */
  def addResult(nbFiles: Int): String = nbFiles + " file(s) added to index."

  /* COMMIT */
  def nothingToCommit(repoPath: String): String = "On branch " + BranchUtil.getCurrentBranchName(repoPath) + "/n/n nothing to commit"

  def commitResult(branch: String, hash: String, message: String): String = "[" + branch + " " + hash.slice(0, 7) + "] " + message + "\n "

  def sameCommitResult(branch: String): String = "On branch " + branch + "\nNothing to commit, working tree clean."

  /* STATUS */
  def statusResult(branch: String,
                   tracked_modified_notAdded: List[String],
                   deleted_notAdded: List[String],
                   tracked_committed_modified: List[String],
                   tracked_neverCommitted: List[String],
                   deleted_notCommitted: List[String],
                   untracked: List[String]): String = {


    "On branch " + branch + ".\n\n" +
      "Changes to be committed:\n\n" +
      GREEN +
      (tracked_modified_notAdded.map("new file: " + _) mkString "\n") + "\n" +
      (tracked_committed_modified.map("modified: " + _) mkString "\n") + "\n" +
      (deleted_notCommitted.map("deleted: " + _) mkString "\n") +
      RESET + "\n\n" +
      "Changes not staged for commit:\n (use \"sgit add<file>...\" to update what will be committed)\n\n" +
      RED +
      (tracked_neverCommitted mkString "\n") + "\n" +
      (deleted_notAdded.map("deleted: " + _) mkString "\n") +
      RESET + "\n\n" +
      "Untracked files:\n (use \"sgit add <file>...\" to include in what will be committed)\n\n" +
      RED +
      (untracked mkString "\n") +
      RESET + "\n\n"
  }

  def statusNoCommit(repoPath: String): String = "On branch " + BranchUtil.getCurrentBranchName(repoPath) + "\n\nNo commits yet."

  /* TAG */
  def tagResult(created: Boolean, name: String): String = {
    if (created) "Tag '" + name + "' created"
    else "fatal: tag '" + name + "' already exists"
  }

  def tagNoCommit() = "fatal: Failed to resolve 'HEAD' as a valid ref (i.e. there is no commit to tag)."

  /* BRANCH */
  def branchResult(created: Boolean, name: String): String = {
    if (created) "Branch '" + name + "' created"
    else "fatal: branch named '" + name + "' already exists"
  }

  def branchNoMaster() = "fatal: Not a valid object name: 'master'."

  def branchAVResult(branches: List[BranchTag], tags: List[BranchTag], currentBranch: String): String = {

    val current = branches.filter(_.name == currentBranch).head
    val resultCurrentBranch = "branches : \n" + Console.GREEN + current.name + " " + current.hash.slice(0, 7) + " " + current.commitMessage + Console.RESET + "\n"
    val resultBranches = resultCurrentBranch + branches.filter(_.name != currentBranch).map(b => b.toString).mkString("\n")

    resultBranches + "\n\ntags : \n" + tags.map(t => t.toString).mkString("\n")
  }

  /* LOG */
  def logNotCommit() = "There is no commit yet"

  def logResult(commits: List[CommitToDiff], option: Option[(List[FilesToDiff], String) => String], repoPath: String): String = {

    @tailrec
    def loop(remainingCommits: List[CommitToDiff], result: String): String = {

      remainingCommits match {

        case Nil => result

        case commitToDiff :: tail =>

          if (option.isEmpty) {
            val resultUpdated = YELLOW + "commit " + commitToDiff.commitHash + RESET + "\n     " + commitToDiff.commitMsg + "\n\n" + result
            loop(tail, resultUpdated)
          }
          else {
            val diffWithParent = option.get(commitToDiff.filesToDiff, repoPath)
            val resultUpdated = YELLOW + "commit " + commitToDiff.commitHash + RESET + "\n     " + commitToDiff.commitMsg + "\n" + diffWithParent + "\n\n" + result
            loop(tail, resultUpdated)
          }
      }
    }

    loop(commits, "")
  }
}
