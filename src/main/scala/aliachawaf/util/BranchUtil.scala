package aliachawaf.util

import java.io.File

/**
 * Class to represent a branch or a tag
 *
 * @param name          : name of the branch or tag
 * @param hash          : hash of the commit it is referencing
 * @param commitMessage : message of the commit it is referencing
 */
class BranchTag(val name: String, val hash: String, val commitMessage: String) {
  override def toString: String = {
    name + " " + hash.slice(0, 7) + " " + commitMessage
  }
}

object BranchUtil {

  /**
   *
   * @param repoPath : path of the SGit repository
   * @return the current branch for the given SGit repository with the format "branches/name"
   */
  def getCurrentBranch(repoPath: String): String = {
    FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "HEAD") mkString "\n"
  }

  /**
   *
   * @param repoPath : path of the SGit repository
   * @return the name of the current branch for the given SGit repository
   */
  def getCurrentBranchName(repoPath: String): String = {
    getCurrentBranch(repoPath).split(File.separator).last
  }

  /**
   *
   * @param typeObject : type of the object we want to get all the existing ones (branches or tags)
   * @param repoPath   : path of the SGit repository
   * @return the list of branches or tags for the given SGit repository
   */
  def getAllBranchesOrTags(typeObject: String, repoPath: String): List[BranchTag] = {

    val objectPath = repoPath + File.separator + ".sgit" + File.separator + typeObject
    val objects = new File(objectPath).listFiles().toList

    objects.map(o => new BranchTag(
      o.getName,
      FileUtil.getFileContent(o.getAbsolutePath).head,
      CommitUtil.getCommitMessage(ObjectUtil.getObjectContent(repoPath, FileUtil.getFileContent(o.getAbsolutePath) mkString ""))
    ))
  }
}