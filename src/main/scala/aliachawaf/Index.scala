package aliachawaf

import java.io.File

import aliachawaf.Repository.{getPathSGit, hasIndexFile}
import aliachawaf.objects.SGitObject
import aliachawaf.util.FileUtil

import scala.annotation.tailrec

object Index {

  /*
  Add a file to the stage in index file.
  For the first add execution, we have to create INDEX file in .sgit
   */
  def add(files: Seq[File], directory: String): Unit = {

    // Get .sgit path
    val sgitParentPath = getPathSGit(directory).get
    val sgitPath = sgitParentPath + File.separator + ".sgit"

    // Create INDEX file if doesnt exists yet
    if (!hasIndexFile(sgitPath)) {
      new File(sgitPath + File.separator + "INDEX").createNewFile()
    }
    // Get all the files corresponding to the arguments input in add command line
    val listFilePaths = FileUtil.getFilePaths(files, directory)

    // Create a Blob for each file and add it in .sgit/objects
    listFilePaths.foreach(p => addBlobInObjects(p.replace(sgitParentPath + File.separator, ""), sgitPath))

    // Add files in .sgit/INDEX
  }

  def addBlobInObjects(filePath: String, sgitPath: String): Unit = {

    // Get file content in order to hash it
    val source = scala.io.Source.fromFile(filePath)
    val lines = try source.getLines mkString "\n" finally source.close()

    // Hash content to use it as a blob's id
    val hashedID = SGitObject.hash(lines)
    val blobPath = sgitPath + File.separator + "objects" + File.separator + hashedID

    // If the file is already indexed and not modified (same hashed id), we do nothing
    if (!isAlreadyIndexed(hashedID, blobPath, sgitPath)) {
      // Create blob in .sgit/objects
      if (!new File(blobPath).exists()) {
        FileUtil.createNewFile(blobPath, lines)
      }

      // Update .sgit/INDEX with the blob
      updateIndex(hashedID, filePath, sgitPath)
    }
  }

  def updateIndex(hashedID: String, filePath: String, sgitPath: String): Unit = {

    val indexPath = sgitPath + File.separator + "INDEX"

    // TO DO : Checks if all the line is contained
    removePathAlreadyIndexed(hashedID, filePath, sgitPath)

    val indexContent = hashedID + " " + filePath + "\n"
    FileUtil.writeFile(new File(indexPath), indexContent.getBytes.toList, append = true)
  }

  /* Remove lines of .sgit/INDEX which contains  */
  def removePathAlreadyIndexed(hashedID: String, filePath: String, sgitPath: String): Unit = {

    // Get INDEX content as a String
    val indexPath = sgitPath + File.separator + "index"
    val source = scala.io.Source.fromFile(indexPath)
    val indexLines = try source.getLines mkString "\n" finally source.close()

    // Check if INDEX content contains the filePath,
    // if yes we have to remove it in order to replace it with the new version (new hashed id)
    if (indexLines.contains(filePath)) {
      val indexLinesList = indexLines.split("\n").toList.filter(line => !line.contains(filePath))
      FileUtil.writeFile(new File(indexPath), "".getBytes.toList, append = false)
      indexLinesList.foreach(line => FileUtil.writeFile(new File(indexPath), (line + "\n").getBytes.toList, append = true))
    }
  }

  /* Return true if the file with the given path is already in .sgit/INDEX with the given hashedID */
  def isAlreadyIndexed(hashedID: String, filePath: String, sgitPath: String): Boolean = {
    // Get INDEX content
    val indexPath = sgitPath + File.separator + "INDEX"
    val source = scala.io.Source.fromFile(indexPath)
    val indexLines = try source.getLines.toList finally source.close()
    // Check if INDEX contains
    indexLines.contains(hashedID + " " + filePath)
  }
}
