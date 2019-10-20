package aliachawaf.util

import java.io.File

object IndexUtil {

  /* Returns true if the given repo contains INDEX file */
  def indexFileExists(repoPath: String): Boolean = new File(repoPath + File.separator + ".sgit" + File.separator + "INDEX").exists()

  def createEmptyIndexFile(repoPath: String) : Unit = new File(repoPath + File.separator + ".sgit" + File.separator + "INDEX").createNewFile()

  def editIndexFile(repoPath: String, content: String, append: Boolean): Unit = {
    val indexPath = repoPath + File.separator + ".sgit" + File.separator + "INDEX"
    FileUtil.writeFile(new File(indexPath), content.getBytes.toList, append = append)
  }


  def getIndexContent(repoPath: String): List[String] = {
    FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")
  }

  def getIndexPaths(repoPath: String): List[String] = {
    getIndexContent(repoPath).map(_.split(" ")(1))
  }

  def getIndexHashes(repoPath: String): List[String] = {
    getIndexContent(repoPath).map(_.split(" ")(0))
  }

  def getIndexAsMap(repoPath: String): Map[String, String] = {
    val pathsIndex = getIndexPaths(repoPath)
    val hashesIndex = getIndexHashes(repoPath)
    (pathsIndex zip hashesIndex).toMap
  }
}
