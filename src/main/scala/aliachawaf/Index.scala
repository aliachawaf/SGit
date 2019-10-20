package aliachawaf

import java.io.File

import aliachawaf.util.{FileUtil, IndexUtil, ObjectUtil, ResultUtil}

class Blob(val filePath: String, val content: String)

object Index {

  /**
   * Add a file to the stage in index file.
   * For the first add execution, we have to create INDEX file in .sgit
   *
   * @param arguments : Files to add, given as a Seq (files name or glob)
   * @param repoPath  : path of the sgit repository
   * @return add result to display to user
   */
  def add(arguments: Seq[String], repoPath: String): String = {

    /*_______________ I/O PART : READING _______________*/

    if (!IndexUtil.indexFileExists(repoPath)) IndexUtil.createEmptyIndexFile(repoPath)

    val indexContent = IndexUtil.getIndexContent(repoPath)

    val filesNotFound = arguments.map(new File(_)).filter(!_.exists()).map(_.getAbsolutePath.replace(repoPath + File.separator, ""))

    val filesToAdd = getBlobsToAdd(repoPath, arguments)


    /*___________________  PURE PART ___________________*/

    // keep only new blobs (not already indexed or with new content)
    val blobsToAdd = filesToAdd.filter(!isAlreadyIndexed(indexContent, _, repoPath))


    /*_______________ I/O PART : WRITING _______________*/

    // Remove files not found/deleted from index
    filesNotFound.foreach(removePathAlreadyIndexed(_, indexContent, repoPath))

    // Create blobs in .sgit/objects
    filesToAdd.foreach(blob => ObjectUtil.addSGitObject(repoPath, blob.content))

    // Update .sgit/INDEX file
    filesToAdd.foreach(updateIndex(_, indexContent, repoPath))

    ResultUtil.addResult(filesToAdd.length)
  }


  /**
   * Get the files we have to add in index
   * @param repoPath  : path of the sgit repo
   * @param arguments : Files argument of add command, given as a Seq of (file names or glob)
   * @return the seq of Blobs we have to add in index
   */
  def getBlobsToAdd(repoPath: String, arguments: Seq[String]): Seq[Blob] = {

    // GET FILES
    val filesCurrentDir = arguments.map(new File(_)).filter(_.isFile)

    // ADD FILES OF DIRECTORIES
    val directoriesCurrentDir = arguments.map(new File(_)).filter(_.isDirectory)
    val allFilesCurrentDir = filesCurrentDir ++ directoriesCurrentDir.flatMap(FileUtil.recursiveListFiles).toList

    // keep only files and remove files from .sgit folder
    val allFilesToAdd =
      allFilesCurrentDir
        .filter(_.isFile)
        .filter(!_.getAbsolutePath.contains(".sgit"))

    allFilesToAdd.map(file => new Blob(file.getAbsolutePath.replace(repoPath + File.separator, ""), FileUtil.getFileContent(file.getAbsolutePath) mkString "\n"))
  }

  /**
   * Update index with the given file
   * (add it in index for the first time or update the hash)
   * @param file : file we want to add
   * @param indexContent : inedx lines
   * @param repoPath : path of the sgit repository
   */
  def updateIndex(file: Blob, indexContent: List[String], repoPath: String): Unit = {

    removePathAlreadyIndexed(file.filePath, indexContent, repoPath)

    val hash = ObjectUtil.hash(file.content)
    val content = hash + " " + file.filePath + "\n"
    IndexUtil.editIndexFile(repoPath, content, append = true)
  }

  def removePathAlreadyIndexed(filePath: String, indexContent: List[String], repoPath: String): Unit = {
    val indexString = indexContent mkString "\n"

    // Check if INDEX content contains the filePath,
    // if yes we have to remove it in order to replace it with the new version (new hashed id)
    if (indexString.contains(filePath)) {
      val indexLinesWithoutFile = indexContent.filterNot(_.split(" ")(1) == filePath)
      if (indexLinesWithoutFile.isEmpty) IndexUtil.editIndexFile(repoPath, "", append = false)
      else IndexUtil.editIndexFile(repoPath, (indexLinesWithoutFile mkString "\n") + "\n", append = false)
    }
  }

  /* Return true if the given file is already in .sgit/INDEX with same content */
  def isAlreadyIndexed(indexContent: List[String], file: Blob, repoPath: String): Boolean = {
    val hashedID = ObjectUtil.hash(file.content)
    indexContent.contains(hashedID + " " + file.filePath)
  }
}