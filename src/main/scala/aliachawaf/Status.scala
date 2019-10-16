package aliachawaf

import java.io.File
import java.nio.file.Paths

import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, IndexUtil, ObjectUtil, ResultUtil}
import aliachawaf.util.IndexUtil._

import scala.reflect.io.Path

object Status {

  def status(currentDir: String): String = {

    val repoPath = Repository.getRepoPath(currentDir).get
    val branch = BranchUtil.getCurrentBranch(repoPath)

    if (CommitUtil.getLastCommit(repoPath, branch).isDefined) {
      ResultUtil.statusResult(
        get_Tracked_Modified_NotAdded(currentDir, repoPath),
        get_Tracked_Committed_Modified(currentDir, repoPath),
        get_Tracked_NeverCommitted(currentDir, repoPath),
        get_Untracked(currentDir, repoPath)
      )
    }
    else ResultUtil.statusNoCommit(repoPath)
  }

  /**
   * @return the list (paths) of the untracked files (not added yet in .sgit/INDEX)
   */
  def get_Untracked(currentDir: String, repoPath: String): List[String] = {

    val allRepoFiles =
      FileUtil.recursiveListFiles(new File(repoPath))
        .filter(_.isFile)
        .filter(!_.getAbsolutePath.contains(".sgit"))
        .map(_.getAbsolutePath.replace(repoPath + File.separator, ""))
        .toList

    val indexContent = getIndexContent(repoPath) mkString "\n"

    val untracked = allRepoFiles.filter(!indexContent.contains(_))
    toRelativePaths(untracked, currentDir, repoPath)
  }

  /**
   * @return the list (paths) of the tracked and modified files (modified after been added in .sgit/INDEX)
   */
  def get_Tracked_Modified_NotAdded(currentDir: String, repoPath: String): List[String] = {
    val indexLines = getIndexContent(repoPath)
    val index = indexLines mkString "\n"

    val tracked = getIndexPaths(repoPath)
    val newHashTracked = tracked.map(path => ObjectUtil.hash(FileUtil.getFileContent(repoPath + File.separator + path) mkString "\n"))
    val mapNewHash = (tracked zip newHashTracked).toMap

    val mapOldHash = getIndexAsMap(repoPath)

    val trackedModifedFiles =
      mapNewHash
        .filter(path => mapOldHash(path._1) != path._2)
        .keys
        .toList

    toRelativePaths(trackedModifedFiles, currentDir, repoPath)
  }


  /**
   * @return the list (paths) of the tracked but never committed before
   */
  def get_Tracked_NeverCommitted(currentDir: String, repoPath: String): List[String] = {

    val indexPaths = getIndexPaths(repoPath)
    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)

    if (lastCommitTree.isEmpty) toRelativePaths(indexPaths, currentDir, repoPath)
    else {
      val list = indexPaths.filter(path => CommitUtil.getBlobHashCommitted(repoPath, path.split(File.separator).toList, lastCommitTree.get).isEmpty)
      toRelativePaths(list, currentDir, repoPath)
    }
  }

  /**
   * @return the list (paths) of the tracked committed and modified files
   *         (ie files added, but different from the last commit)
   */
  def get_Tracked_Committed_Modified(currentDir: String, repoPath: String): List[String] = {

    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)

    if (lastCommitTree.isEmpty) List()
    else {

      // get hash of files already committed before
      val indexPaths = getIndexPaths(repoPath)
      val commitHashes = indexPaths.map(path => CommitUtil.getBlobHashCommitted(repoPath, path.split(File.separator).toList, lastCommitTree.get).getOrElse("never committed"))

      val commitMapAllFiles = (indexPaths zip commitHashes).toMap
      val commitMap = commitMapAllFiles.filter(_._2 != "never committed")

      // keep from the files already committed those were modified after (hash in index is different in commit)
      val indexMap = getIndexAsMap(repoPath)
      val list = commitMap.filter(element => indexMap(element._1) != element._2).keys.toList
      toRelativePaths(list, currentDir, repoPath)
    }
  }

  /**
   *
   * @return the files (relative paths) tracked but not present in working tree
   *         ( ie removed from sgit repository, but still in index)
   */
  def get_Deleted_NotAdded(currentDir: String, repoPath: String): List[String] = {
    val indexPathsNotFound = getIndexPaths(repoPath).filter(!new File(_).exists())
    toRelativePaths(indexPathsNotFound, currentDir, repoPath)
  }

  /**
   *
   * @return the files deleted (removed from index) but still in commit tree
   */
  def get_Deleted_NotCommitted(currentDir: String, repoPath: String): List[String] = {

    val branch = BranchUtil.getCurrentBranchName(repoPath)
    val lastCommitTree = CommitUtil.getLastCommit(repoPath, branch)

    if (lastCommitTree.isEmpty) List()
    else {
      val commitTreeAsMap = CommitUtil.getCommitAsMap(repoPath, lastCommitTree.get)
      val indexAsMap = IndexUtil.getIndexAsMap(repoPath)

      val list = commitTreeAsMap.keys.toList diff indexAsMap.keys.toList
      toRelativePaths(list, currentDir, repoPath)
    }
  }

  def toRelativePaths(paths: List[String], currentDir: String, repoPath: String): List[String] = {
    paths.map(p => Paths.get(currentDir).relativize(Paths.get(repoPath + File.separator + p)).toString)
  }
}