package aliachawaf

import java.io.File
import util.FileUtil

class Repository(path: String)

object Repository {

  /* Initializes the repository. Creates the necessary folder structure and files. */
  def initialize(path: String): Unit = {

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

    } else {
      println("Already initialized SGit repository")
    }
  }

  /* Returns true if the given repository is already initialized. */
  def isInitialized(path: String): Boolean = new File(path + File.separator + ".sgit").exists()

  /* Returns true if the path is in a SGit repository */
  def isInRepository(): Boolean = Repository.getRepoPath(System.getProperty("user.dir")).isDefined

  /* Returns true if the given path contains INDEX file */
  def hasIndexFile(): Boolean = new File(getRepoPath(System.getProperty("user.dir")).get + ".sgit" + File.separator + "INDEX").exists()

  /* Returns the path containing .sgit folder if exists,
     else returns None if the given path in parameter is not in a SGit repository
  */
  def getRepoPath(path: String): Option[String] = {
    if (path.isEmpty) None
    else if (isInitialized(path)) Some(path)
    else {
      val parentFile = new File(path).getParentFile
      if (!parentFile.getName.isEmpty) getRepoPath(parentFile.getAbsolutePath)
      else getRepoPath("")
    }
  }
}