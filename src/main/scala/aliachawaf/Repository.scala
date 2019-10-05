package aliachawaf

import java.io.File
import util.FileUtil
import scala.annotation.tailrec

class Repository(path: String)

object Repository {

  /* Initializes the repository. Creates the necessary folder structure and files. */
  def initialize(path: String): Unit = {

    if (!isInitialized(path)) {

      // SGit path for the folder structure : path/.sgit/
      val pathSGit = path + File.separator +  ".sgit" + File.separator

      // Make .sgit directory
      new File(pathSGit).mkdir()

      // Add folders in .sgit
      val folders = List("branches", "trees", "blobs", "tags", "commits")
      folders.foreach(folder => new File(pathSGit + folder).mkdir())

      // Add HEAD file in .sgit
      FileUtil.createNewFile(pathSGit + "HEAD", "branches/master")

    } else {
      println("Already initialized SGit repository")
    }
  }

  /* Returns true if the given repository is already initialized. */
  def isInitialized(path: String): Boolean = new File(path + File.separator + ".sgit").exists()

  /* Returns true if the given path contains INDEX file */
  def hasIndexFile(path: String): Boolean = new File(path +  ".sgit" + File.separator + "INDEX").exists()

  /* Returns the path containing .sgit folder if exists,
     else returns None if the given path in parameter is not in a SGit repository
  */
  def getPathSGit(path: String): Option[String] = {
    @tailrec
    def loop(currentPath: String): Option[String] = {

      if (currentPath.isEmpty)
        None
      else if (isInitialized(currentPath))
        Some(currentPath)
      else {
        val pathArray = currentPath.split(File.separator)
        val parentPath = pathArray.take(pathArray.length - 1).mkString(File.separator)
        loop(parentPath)
      }
    }
    return loop(path)
  }

  /*
  Add a file to the stage in index file.
  For the first add execution, we have to create INDEX file in .sgit
   */
  def add(parameter: String, path: String): Unit = {
    if (!hasIndexFile(path)) {
      new File(path + File.separator +  ".sgit" + File.separator + "INDEX").createNewFile()
    }
  }
}