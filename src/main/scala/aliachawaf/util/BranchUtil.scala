package aliachawaf.util

import java.io.File


class BranchTag(val name: String, val hash: String, val commitMessage: String) {
  override def toString: String = {
    name + " " + hash.slice(0,7) + " " + commitMessage
  }
}

object BranchUtil {

  def getCurrentBranch(repoPath: String): String = {
    FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "HEAD") mkString "\n"
  }

  def getCurrentBranchName(repoPath: String): String = {
    getCurrentBranch(repoPath).split(File.separator).last
  }


  def getAllBranchesOrTags(typeObject: String, repoPath: String) : List[BranchTag] = {

    val objectPath = repoPath + File.separator + ".sgit" + File.separator + typeObject
    val objects = new File(objectPath).listFiles().toList

    objects.map(o => new BranchTag(
      o.getName,
      FileUtil.getFileContent(o.getAbsolutePath).head,
      CommitUtil.getCommitMessage(ObjectUtil.getObjectContent(repoPath, FileUtil.getFileContent(o.getAbsolutePath) mkString ""))
      ))
  }

}
