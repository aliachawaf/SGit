package aliachawaf.command

import java.io.File

import aliachawaf.util.FileUtil
import aliachawaf.util.RepoUtil.isInitialized
import aliachawaf.util.ResultUtil.initResult

object Init {

  /**
   * Initializes the repository. Creates the necessary folder structure and files.
   * @param path : the path of the directory to be initialized
   * @return true if the directory wad initialized, false if already initialized before
   */
  def initialize(path: String): String = {

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

      initResult(true, path)
    }
    else initResult(false, path)
  }

}
