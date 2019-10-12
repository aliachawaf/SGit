package aliachawaf

import java.io.File
import util.{FileUtil, PrintUtil}
import scala.annotation.tailrec

class Repository(path: String)

object Repository {

  /**
   * Initializes the repository. Creates the necessary folder structure and files.
   * @param path : the path of the directory to be initialized
   * @return true if the directory wad initialized, false if already initialized before
   */
  def initialize(path: String): Boolean = {

    if (!isInitialized(path)) {

      // SGit path for the folder structure : path/.sgit/
      val pathSGit = path + File.separator + ".sgit" + File.separator

      // Make .sgit directory
      new File(pathSGit).mkdir()

      // Add folders in .sgit
      val folders = List("branches", "tags", "objects")
      folders.foreach(folder => new File(pathSGit + folder).mkdir())

      // Add HEAD file in .sgit
      FileUtil.createNewFile(pathSGit + "HEAD", "branches/master")

      true
    }
    else false
  }

  /* Returns true if the given repository is already initialized. */
  def isInitialized(path: String): Boolean = new File(path + File.separator + ".sgit").exists()

  /* Returns true if the path is in a SGit repository */
  def isInRepository(currentDirectory: String): Boolean = Repository.getRepoPath(currentDirectory).isDefined

  /* Returns true if the given path contains INDEX file */
  def hasIndexFile(repoPath: String): Boolean = new File(repoPath + File.separator + ".sgit" + File.separator + "INDEX").exists()

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