package aliachawaf

import java.io.File
import java.nio.file.Paths

import aliachawaf.Status.toRelativePaths
import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, IndexUtil, ObjectUtil, ResultUtil}
import aliachawaf.util.IndexUtil._

import scala.reflect.io.Path

object Status {

  def status(currentDir: String): String = {

    /*___________________  I/O PART : READING  ____________________*/

    val repoPath = Repository.getRepoPath(currentDir).get
    val branch = BranchUtil.getCurrentBranchName(repoPath)

    val allRepoFiles = getAllRepoFiles(repoPath)
    val indexMap = IndexUtil.getIndexAsMap(repoPath)

    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)
    val commitTree = CommitUtil.getCommitAsMap(repoPath, lastCommitTree)


    /*___________________  PURE FUNCTIONAL PART ___________________*/

    if (lastCommitTree.isEmpty) ResultUtil.statusNoCommit(repoPath)

    else {
      ResultUtil.statusResult(
        branch,
        get_Tracked_Modified_NotAdded(allRepoFiles, indexMap, currentDir, repoPath),
        get_Deleted_NotAdded(allRepoFiles, indexMap, currentDir, repoPath),
        get_Tracked_Committed_Modified(commitTree, indexMap, currentDir, repoPath),
        get_Deleted_NotCommitted(commitTree, indexMap, currentDir, repoPath),
        get_Tracked_NeverCommitted(commitTree, indexMap, currentDir, repoPath),
        get_Untracked(allRepoFiles, indexMap, currentDir, repoPath)
      )
    }
  }

  def getAllRepoFiles(repoPath: String): Map[String, List[String]] = {

    val allRepoFilesPaths =
      FileUtil.recursiveListFiles(new File(repoPath))
        .filter(_.isFile)
        .filter(!_.getAbsolutePath.contains(".sgit"))
        .map(_.getAbsolutePath.replace(repoPath + File.separator, ""))
        .toList

    val allRepoFilesContent = allRepoFilesPaths.map(path => FileUtil.getFileContent(repoPath + File.separator + path))

    (allRepoFilesPaths zip allRepoFilesContent).toMap
  }

  /**
   * @return the list (paths) of the untracked files (not added yet in .sgit/INDEX)
   */
  def get_Untracked(allRepoFiles: Map[String, List[String]], indexMap: Map[String, String], currentDir: String, repoPath: String): List[String] = {

    val indexPaths = indexMap.keys.toList
    val filesPaths = allRepoFiles.keys.toList

    val untracked = filesPaths.filter(!indexPaths.contains(_))
    toRelativePaths(untracked, currentDir, repoPath)
  }

  /**
   * @return the list (paths) of the tracked and modified files (modified after been added in .sgit/INDEX)
   */
  def get_Tracked_Modified_NotAdded(allRepoFiles: Map[String, List[String]], indexMap: Map[String, String], currentDir: String, repoPath: String): List[String] = {

    val tracked = indexMap.keys.toList
    val newHashTracked = tracked.map(allRepoFiles(_))
    val lastVersionMap = (tracked zip newHashTracked).toMap

    val trackedModifiedFiles = lastVersionMap.filter(path => indexMap(path._1) != ObjectUtil.hash(path._2 mkString "\n"))
    toRelativePaths(trackedModifiedFiles.keys.toList, currentDir, repoPath)
  }


  /**
   * @return the list (paths) of the tracked but never committed before
   */
  //def get_Tracked_NeverCommitted(lastCommitTree: Option[String], allRepoFiles: Map[String, List[String]], indexMap: Map[String, String], currentDir: String, repoPath: String): List[String] = {
  def get_Tracked_NeverCommitted(commitTree: Option[Map[String, List[String]]], indexMap: Map[String, String], currentDir: String, repoPath: String): List[String] = {

    println("YES")
    if (commitTree.isEmpty) indexMap.keys.toList
    else {
      println("YES2")
      val paths_Tracked_NotCommitted = indexMap.keys.toList diff commitTree.get.keys.toList
      println(paths_Tracked_NotCommitted)
      val test = toRelativePaths(paths_Tracked_NotCommitted, currentDir, repoPath)
      println(test)
      test
    }
  }

  /**
   * @return the list (paths) of the tracked committed and modified files
   *         (ie files added, but different from the last commit)
   */
  def get_Tracked_Committed_Modified(commitTree: Option[Map[String, List[String]]], indexMap: Map[String, String], currentDir: String, repoPath: String): List[String] = {

    if (commitTree.isEmpty) List()
    else {
      // get hash of files already committed before
      val commitMap = (indexMap.keys.toList zip commitTree.get.values.toList).toMap

      // keep from the files already committed those were modified after (hash in index is different in commit)
      val list = commitMap.filter(element => indexMap(element._1) != ObjectUtil.hash(element._2 mkString "\n")).keys.toList
      toRelativePaths(list, currentDir, repoPath)
    }
  }


  /**
   *
   * @return the files (relative paths) tracked but not present in working tree
   *         ( ie removed from sgit repository, but still in index)
   */
  def get_Deleted_NotAdded(allRepoFiles: Map[String, List[String]], indexContent: Map[String, String], currentDir: String, repoPath: String): List[String] = {
    val indexPathsNotFound =  indexContent.keys.toList diff allRepoFiles.keys.toList
    toRelativePaths(indexPathsNotFound, currentDir, repoPath)
  }


  /**
   *
   * @return the files deleted (removed from index) but still in commit tree
   */
  def get_Deleted_NotCommitted(commitTree: Option[Map[String, List[String]]], indexContent: Map[String, String], currentDir: String, repoPath: String): List[String] = {

    println("YES")
    if (commitTree.isEmpty) List()
    else {
      val list = commitTree.get.keys.toList diff indexContent.keys.toList
      println("LSIT " + list)
      toRelativePaths(list, currentDir, repoPath)
    }
  }

  def toRelativePaths(paths: List[String], currentDir: String, repoPath: String): List[String] = {
    paths.map(p => Paths.get(currentDir).relativize(Paths.get(repoPath + File.separator + p)).toString)
  }
}