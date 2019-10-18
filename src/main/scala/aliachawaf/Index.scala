package aliachawaf

import java.io.File

import aliachawaf.util.{FileUtil, IndexUtil, ObjectUtil, ResultUtil}

object Index {

  /**
   * Add a file to the stage in index file.
   * For the first add execution, we have to create INDEX file in .sgit
   *
   * @param arguments : Files to add, given as a Seq (files name or glob)
   * @param repoPath : path of the sgit repository
   * @return add result
   */
  def add(arguments: Seq[String], repoPath: String): String = {

    val sgitPath = repoPath + File.separator + ".sgit"

    // Create INDEX file if doesnt exists yet
    if (!Repository.hasIndexFile(repoPath)) {
      new File(sgitPath + File.separator + "INDEX").createNewFile()
    }

    val filesCurrentDirectory = arguments.map(new File(_)).filter(_.isFile)
    val directoriesCurrentDirectory = arguments.map(new File(_)).filter(_.isDirectory)

    val allFilesCurrentDirectory = filesCurrentDirectory ++ directoriesCurrentDirectory.flatMap(FileUtil.recursiveListFiles).toList

    // keep only files and remove files from .sgit folder
    val allFilesToAdd = allFilesCurrentDirectory.filter(_.isFile).filter(!_.getAbsolutePath.contains(".sgit"))

    // remove absolute path and keep relative path
    val allFilesPaths = allFilesToAdd.map(_.getAbsolutePath.replace(repoPath + File.separator, ""))

    // Create blobs in .sgit/objects and add them in .sgit/INDEX file
    allFilesPaths.foreach(addBlobInObjects(_, repoPath))

    val filesNotFound = arguments.map(new File(_)).filter(!_.exists()).map(_.getAbsolutePath.replace(repoPath + File.separator, ""))
    filesNotFound.foreach(removePathAlreadyIndexed(_, repoPath))

    ResultUtil.addResult(allFilesPaths.length)
  }

  def addBlobInObjects(filePath: String, repoPath: String): Unit = {

    // Get file content in order to hash it
    val lines = FileUtil.getFileContent(repoPath + File.separator + filePath).mkString

    // Hash content to use it as a blob's id
    val hashedID = ObjectUtil.hash(lines)
    val blobPath = repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + hashedID

    // If the file is already indexed and not modified (same hashed id), we do nothing
    if (!isAlreadyIndexed(hashedID, filePath, repoPath)) {
      // Create blob in .sgit/objects
      if (!new File(blobPath).exists()) {
        FileUtil.createNewFile(blobPath, lines)
      }

      // Update .sgit/INDEX with the blob
      updateIndex(hashedID, filePath, repoPath)
    }
  }

  def updateIndex(hashedID: String, filePath: String, repoPath: String): Unit = {

    val indexPath = repoPath + File.separator + ".sgit" + File.separator + "INDEX"
    removePathAlreadyIndexed(filePath, repoPath)

    val indexContent = hashedID + " " + filePath + "\n"
    FileUtil.writeFile(new File(indexPath), indexContent.getBytes.toList, append = true)
  }

  /* Remove lines of .sgit/INDEX which contains  */
  def removePathAlreadyIndexed(filePath: String, repoPath: String): Unit = {
    // Get INDEX content as a String
    val indexPath = repoPath + File.separator + ".sgit" + File.separator + "INDEX"
    val indexLines = IndexUtil.getIndexContent(repoPath)
    val indexString = indexLines mkString "\n"

    // Check if INDEX content contains the filePath,
    // if yes we have to remove it in order to replace it with the new version (new hashed id)
    if (indexString.contains(filePath)) {
      val indexLinesWithoutFile = indexLines.filterNot(_.split(" ")(1) == filePath)
      if (indexLinesWithoutFile.isEmpty) FileUtil.writeFile(new File(indexPath), "".getBytes.toList, append = false)
      else FileUtil.writeFile(new File(indexPath), ((indexLinesWithoutFile mkString "\n") + "\n").getBytes.toList, append = false)
    }
  }

  /* Return true if the file with the given path is already in .sgit/INDEX with the given hashedID */
  def isAlreadyIndexed(hashedID: String, filePath: String, repoPath: String): Boolean = {
    // Get INDEX content
    val indexLines = IndexUtil.getIndexContent(repoPath)

    // Check if INDEX contains
    indexLines.contains(hashedID + " " + filePath)
  }
}