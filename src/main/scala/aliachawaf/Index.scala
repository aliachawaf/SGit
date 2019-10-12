package aliachawaf

import java.io.File
import aliachawaf.util.{FileUtil, ObjectUtil}

object Index {

  /*
    Add a file to the stage in index file.
    For the first add execution, we have to create INDEX file in .sgit
     */
  def add(arguments: Seq[String], directory: String, repoPath: String): Unit = {

    val sgitPath = repoPath + File.separator + ".sgit"

    // Create INDEX file if doesnt exists yet
    if (!Repository.hasIndexFile(repoPath)) {
      new File(sgitPath + File.separator + "INDEX").createNewFile()
    }

    val filesCurrentDirectory = arguments.map(f => new File(f)).filter(_.isFile)
    val directoriesCurrentDirectory = arguments.map(f => new File(f)).filter(_.isDirectory)

    val allFilesCurrentDirectory = filesCurrentDirectory ++ directoriesCurrentDirectory.flatMap(FileUtil.recursiveListFiles).toList

    // keep only files and remove files from .sgit folder
    val allFilesToAdd = allFilesCurrentDirectory.filter(_.isFile).filter(!_.getAbsolutePath.contains(".sgit"))

    // remove absolute path and keep relative path
    val allFilesPaths = allFilesToAdd.map(_.getAbsolutePath.replace(repoPath + File.separator, ""))

    // Create blobs in .sgit/objects and add them in .sgit/INDEX file
    allFilesPaths.foreach(path => addBlobInObjects(path, repoPath))
  }

  def addBlobInObjects(filePath: String, repoPath: String): Unit = {

    // Get file content in order to hash it
    //val fileAbsolutePath = repoPath + File.separator + filePath
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

    val indexPath = repoPath + File.separator + ".sgit" + File.separator + File.separator + "INDEX"
    removePathAlreadyIndexed(hashedID, filePath, repoPath)

    val indexContent = hashedID + " " + filePath + "\n"
    FileUtil.writeFile(new File(indexPath), indexContent.getBytes.toList, append = true)
  }

  /* Remove lines of .sgit/INDEX which contains  */
  def removePathAlreadyIndexed(hashedID: String, filePath: String, repoPath: String): Unit = {

    // Get INDEX content as a String
    val indexPath = repoPath + File.separator + ".sgit" + File.separator + File.separator + "INDEX"
    val indexLines = FileUtil.getFileContent(indexPath).mkString

    // Check if INDEX content contains the filePath,
    // if yes we have to remove it in order to replace it with the new version (new hashed id)
    if (indexLines.contains(filePath)) {
      val indexLinesList = indexLines.split("\n").toList.filter(line => !line.contains(filePath)) mkString "\n"
      FileUtil.writeFile(new File(indexPath), "".getBytes.toList, append = false)
      FileUtil.writeFile(new File(indexPath), indexLinesList.getBytes.toList, append = true)
    }
  }

  /* Return true if the file with the given path is already in .sgit/INDEX with the given hashedID */
  def isAlreadyIndexed(hashedID: String, filePath: String, repoPath: String): Boolean = {
    // Get INDEX content
    val indexPath = repoPath + File.separator + ".sgit" + File.separator + File.separator + "INDEX"
    val indexLines = FileUtil.getFileContent(indexPath)

    // Check if INDEX contains
    indexLines.contains(hashedID + " " + filePath)
  }
}