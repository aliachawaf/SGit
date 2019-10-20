package aliachawaf.util

import java.io.File

object IndexUtil {

  /**
   *
   * @param repoPath : path of the SGit repository
   * @return true if the given SGit repository contains an INDEX file in .sgit
   */
  def indexFileExists(repoPath: String): Boolean = new File(repoPath + File.separator + ".sgit" + File.separator + "INDEX").exists()

  /**
   * Create an empty INDEX file in .sgit folder of the repository
   *
   * @param repoPath : path of the SGit repository
   */
  def createEmptyIndexFile(repoPath: String): Unit = new File(repoPath + File.separator + ".sgit" + File.separator + "INDEX").createNewFile()

  /**
   * Edit INDEX file of the given SGit repository with the given content
   *
   * @param repoPath : path of the SGit repository
   * @param content  : new content to write
   * @param append   : true if we want to append the given content, false to overwrite
   */
  def editIndexFile(repoPath: String, content: String, append: Boolean): Unit = {
    val indexPath = repoPath + File.separator + ".sgit" + File.separator + "INDEX"
    FileUtil.writeFile(new File(indexPath), content.getBytes.toList, append = append)
  }

  /**
   *
   * @param repoPath : path of the SGit repository
   * @return the content of the INDEX file of the given SGit repository (as a list of its lines)
   */
  def getIndexContent(repoPath: String): List[String] = {
    FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")
  }

  /**
   *
   * @param repoPath : path of the SGit repository
   * @return the file paths contained in INDEX file of the given SGit repository
   */
  def getIndexPaths(repoPath: String): List[String] = {
    getIndexContent(repoPath).map(_.split(" ")(1))
  }

  /**
   *
   * @param repoPath : path of the SGit repository
   * @return the hashes contained in INDEX file of the given SGit repository
   */
  def getIndexHashes(repoPath: String): List[String] = {
    getIndexContent(repoPath).map(_.split(" ")(0))
  }

  /**
   *
   * @param repoPath : path of the SGit repository
   * @return the content of INDEX file of the given SGit repository as Map (filePath -> hash)
   */
  def getIndexAsMap(repoPath: String): Map[String, String] = {
    val pathsIndex = getIndexPaths(repoPath)
    val hashesIndex = getIndexHashes(repoPath)
    (pathsIndex zip hashesIndex).toMap
  }
}
