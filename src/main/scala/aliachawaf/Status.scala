package aliachawaf

import java.io.File

import aliachawaf.util.{CommitUtil, FileUtil, ObjectUtil, IndexUtil}
import aliachawaf.util.IndexUtil._

object Status {

  def status(currentDir: String): String = {

    val repoPath = Repository.getRepoPath(currentDir).get

    "Changes to be committed:\n\n" +
      "new file: " + (get_Tracked_Modified_NotAdded(repoPath).mkString("\nnew file: ")) +
      "modified: " + (get_Tracked_Committed_Modified(repoPath).mkString("\nmodified: ")) +
      "\n\n" +
    "Changes not staged for commit:\n (use \"sgit add<file>...\" to update what will be committed)\n\n" +
      (get_Tracked_NeverCommitted(repoPath) mkString "\n") +
      "\n\n" +
    "Untracked files:\n (use \"sgit add <file>...\" to include in what will be committed)\n\n" +
    (get_Untracked(repoPath) mkString "\n")
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

    val mapOldHash = getIndexAsMap(repoPath)

    val trackedModifedFiles = mapNewHash.filter(path => mapOldHash(path._1) != path._2)
    trackedModifedFiles.keys.toList
  }


  /**
   * @return the list (paths) of the tracked and modified files (modified after been added in .sgit/INDEX)
   */
  def get_Tracked_NeverCommitted(repoPath: String): List[String] = {

    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)

    if (lastCommitTree.isEmpty) List()
    else {
      val indexPaths = getIndexPaths(repoPath)
      indexPaths.filter(path => CommitUtil.getBlobHashCommitted(repoPath, path.split(File.separator).toList, lastCommitTree.get).isEmpty)
    }
  }


  def get_Tracked_Committed_Modified(repoPath: String): List[String] = {

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
      commitMap.filter(element => indexMap(element._1) != element._2).keys.toList
    }
  }

  def getAllRepoFiles(repoPath: String) : List[String] = {
    FileUtil.recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath.replace(repoPath + File.separator, ""))
      .toList
  }

}
