package aliachawaf

import java.io.File

import aliachawaf.util.{CommitUtil, FileUtil, ObjectUtil, IndexUtil}
import aliachawaf.util.IndexUtil._

object Status {

  def status(currentDir: String): String = {

    val repoPath = Repository.getRepoPath(currentDir).get

    "TO DO"
  }

  /**
   * @return the list (paths) of the untracked files (not added yet in .sgit/INDEX)
   */
  def get_Untracked(repoPath: String): List[String] = {

    val allRepoFiles = getAllRepoFiles(repoPath)
    val indexContent = getIndexContent(repoPath) mkString "\n"

    allRepoFiles.filter(!indexContent.contains(_))
  }

  /**
   * @return the list (paths) of the tracked and modified files (modified after been added in .sgit/INDEX)
   */
  def get_Tracked_Modified_NotAdded(repoPath: String): List[String] = {

    val indexLines = getIndexContent(repoPath)
    val index = indexLines mkString "\n"

    val allRepoFiles = getAllRepoFiles(repoPath)

    val tracked = allRepoFiles.filter(index.contains(_))
    val newHashTracked = tracked.map(path => ObjectUtil.hash(FileUtil.getFileContent(path) mkString "\n"))
    val mapNewHash = (tracked zip newHashTracked).toMap

    val pathsIndex = getIndexPaths(repoPath)
    val hashesIndex = getIndexHashes(repoPath)
    val mapOldHash = (pathsIndex zip hashesIndex).toMap

    val trackedModifedFiles = mapNewHash.filter(path => mapOldHash(path._1) != path._2)
    trackedModifedFiles.keys.toList
  }


  /**
   * @return the list (paths) of the tracked and modified files (modified after been added in .sgit/INDEX)
   */
  def get_Tracked_NeverCommitted(repoPath: String): List[String] = {

    val indexPaths = getIndexPaths(repoPath)

    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)

    if (lastCommitTree.isDefined) List()
    else indexPaths.filter(path => CommitUtil.getBlobHashCommitted(repoPath, path.split(File.separator).toList, lastCommitTree.get).isEmpty)
  }

  def get_Tracked_Committed_Modified(repoPath: String): List[String] = {

  }

  def getAllRepoFiles(repoPath: String) : List[String] = {
    FileUtil.recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath.replace(repoPath + File.separator, ""))
      .toList
  }

}
