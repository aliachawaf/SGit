package aliachawaf.util

import java.io.File

object IndexUtil {


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
