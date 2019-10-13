package aliachawaf

import java.io.File
import java.nio.file.Paths

import aliachawaf.util.{CommitUtil, FileUtil, IndexUtil, ObjectUtil, ResultUtil}
import aliachawaf.util.IndexUtil._

import scala.reflect.io.Path

object Status {

  def status(currentDir: String): String = {

    ResultUtil.statusResult(
      get_Tracked_Modified_NotAdded(currentDir),
      get_Tracked_Committed_Modified(currentDir),
      get_Tracked_NeverCommitted(currentDir),
      get_Untracked(currentDir)
    )
  }

  /**
   * @return the list (paths) of the untracked files (not added yet in .sgit/INDEX)
   */
  def get_Untracked(currentDir: String): List[String] = {

    val repoPath = Repository.getRepoPath(currentDir).get
    val allRepoFiles = getAllRepoFiles(repoPath)
    val indexContent = getIndexContent(repoPath) mkString "\n"

    val untracked = allRepoFiles.filter(!indexContent.contains(_)).map(repoPath + File.separator + _)
    toRelativePaths(untracked, currentDir)
  }

  /**
   * @return the list (paths) of the tracked and modified files (modified after been added in .sgit/INDEX)
   */
  def get_Tracked_Modified_NotAdded(currentDir: String): List[String] = {

    val repoPath = Repository.getRepoPath(currentDir).get
    val indexLines = getIndexContent(repoPath)
    val index = indexLines mkString "\n"

    val allRepoFiles = getAllRepoFiles(repoPath)

    val tracked = allRepoFiles.filter(index.contains(_))

    val newHashTracked = tracked.map(path => ObjectUtil.hash(FileUtil.getFileContent(repoPath + File.separator + path) mkString "\n"))
    val mapNewHash = (tracked zip newHashTracked).toMap

    val mapOldHash = getIndexAsMap(repoPath)

    val trackedModifedFiles = mapNewHash.filter(path => mapOldHash(path._1) != path._2)
    toRelativePaths(trackedModifedFiles.keys.toList.map(repoPath + File.separator + _), currentDir)
  }


  /**
   * @return the list (paths) of the tracked but never committed before
   */
  def get_Tracked_NeverCommitted(currentDir: String): List[String] = {

    val repoPath = Repository.getRepoPath(currentDir).get
    val indexPaths = getIndexPaths(repoPath)
    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)

    if (lastCommitTree.isEmpty) toRelativePaths(indexPaths.map(repoPath + File.separator + _), currentDir)
    else {
      val list = indexPaths.filter(path => CommitUtil.getBlobHashCommitted(repoPath, path.split(File.separator).toList, lastCommitTree.get).isEmpty)
      toRelativePaths(list.map(repoPath + File.separator + _), currentDir)
    }
  }

  /**
   * @return the list (paths) of the tracked committed and modified files
   */
  def get_Tracked_Committed_Modified(currentDir: String): List[String] = {

    val repoPath = Repository.getRepoPath(currentDir).get
    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)

    if (lastCommitTree.isEmpty) List()
    else {

      // get hash of files already committed before
      val indexPaths = getIndexPaths(repoPath)
      val commitHashes = indexPaths.map(path => CommitUtil.getBlobHashCommitted(repoPath, path.split(File.separator).toList, lastCommitTree.get).getOrElse(""))

      val commitMapAllFiles = (indexPaths zip commitHashes).toMap
      val commitMap = commitMapAllFiles.filter(_._2 != "")

      // keep from the files already committed those were modified after (hash in index is different in commit)
      val indexMap = getIndexAsMap(repoPath)
      val list = commitMap.filter(element => indexMap(element._1) != element._2).keys.toList.map(repoPath + File.separator + _)
      toRelativePaths(list, currentDir)
    }
  }

  def getAllRepoFiles(currentDir: String): List[String] = {

    val repoPath = Repository.getRepoPath(currentDir).get
    FileUtil.recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath.replace(repoPath + File.separator, ""))
      .toList
  }

  def toRelativePaths(absolutePaths: List[String], currentDir: String) : List[String] = {
    absolutePaths.map(p => Paths.get(currentDir).relativize(Paths.get((new File(p)).getAbsolutePath)).toString)
  }
}