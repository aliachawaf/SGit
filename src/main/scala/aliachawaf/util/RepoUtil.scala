package aliachawaf.util

import java.io.File

import scala.annotation.tailrec

class RepoUtil(path: String)

object RepoUtil {

  /* Returns true if the given repository is already initialized. */
  def isInitialized(path: String): Boolean = new File(path + File.separator + ".sgit").exists()

  /* Returns true if the path is in a SGit repository */
  def isInRepository(currentDirectory: String): Boolean = getRepoPath(currentDirectory).isDefined

  /* Returns the path containing .sgit folder if exists,
     else returns None if the given path in parameter is not in a SGit repository
  */
  @tailrec
  def getRepoPath(currentDirectory: String): Option[String] = {
    if (currentDirectory.isEmpty) None
    else if (isInitialized(currentDirectory)) Some(currentDirectory)
    else {
      val parentFile = new File(currentDirectory).getParentFile
      if (!parentFile.getName.isEmpty) getRepoPath(parentFile.getAbsolutePath)
      else getRepoPath("")
    }
  }
}