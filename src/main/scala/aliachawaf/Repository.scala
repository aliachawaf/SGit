package aliachawaf

import java.io.File
import util.FileUtil

class Repository(path: String)

object Repository {

  /* Returns true if the given repository is already initialized. */
  def isInitialized(path: String): Boolean = new File(path + File.separator + ".sgit").exists()

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
}