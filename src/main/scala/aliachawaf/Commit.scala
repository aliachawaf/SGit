package aliachawaf

import java.io.File
import aliachawaf.util.{FileUtil, ObjectUtil}
import scala.annotation.tailrec

object Commit {

  /**
   *
   * @param repoPath : path of the sgit repository
   * @return
   */
  def commit(repoPath: String, commitMsg: String): String = {

    // Get .sgit/INDEX content
    val indexLines = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")

    // Keep only paths form index lines and split by path elements
    val indexPaths = indexLines.map(l => l.split(" ")(1).split(File.separator))

    // Sort index content by descending length
    val indexPathsSorted = indexPaths.sortBy(path => path.length).reverse

    // Create Map index
    val keys = indexLines.map(line => line.split(" ")(1))
    val values = indexLines.map(line => line.split(" ")(0))
    val indexContent = (keys zip values).toMap

    //
    val commitTreeHash = createCommitTree(repoPath, indexPathsSorted, indexContent, indexPathsSorted.head.length)

    // Add commit object in .sgit/objects
    addCommitInObjects(repoPath, commitTreeHash, commitMsg)

    "TO DO"
  }

  /**
   *
   * @param repoPath
   * @param commitTreeHash : hash of the new tree created with the commit
   * @param commitMsg      : message of the commit
   * @return the hash of the new sgit object commit created
   */
  def addCommitInObjects(repoPath: String, commitTreeHash: String, commitMsg: String): String = {

    /** Commit content :
     *    - parent commit hash
     *    - commit tree hash
     *    - commit message
     */

    // Get parent commit
    val currentBranch = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "HEAD") mkString "\n"
    val currentBranchPath = repoPath + File.separator + ".sgit" + File.separator + currentBranch

    println(currentBranchPath)

    if (new File(currentBranchPath).exists()) {
      val parentCommit = FileUtil.getFileContent(currentBranchPath) mkString "\n"
      val commitContent = "parent " + parentCommit + "\n" + "tree " + commitTreeHash + "\n" + "message " + commitMsg
      val commitHash = ObjectUtil.addSGitObject(repoPath, commitContent)
      // Update current branch reference
      FileUtil.createNewFile(currentBranchPath, commitHash)
      commitHash
    }
    // If it is the first commit of the branch, then the commit has no parent
    else {
      val commitContent = "tree " + commitTreeHash + "\n" + "message " + commitMsg
      val commitHash = ObjectUtil.addSGitObject(repoPath, commitContent)
      // Update current branch reference
      FileUtil.createNewFile(currentBranchPath, commitHash)
      commitHash
    }
  }


  /**
   * @return The hash of the Commit Tree created recursively
   * @param repoPath         : path of the SGit repository
   * @param indexPaths       : List of paths present in INDEX. Each path is split by separator in an Array and sorted by descending path length
   * @param indexContent     : Map containing the lines of INDEX with 'paths as keys' and 'hashes as values'
   * @param currentPathDepth : current depth of paths we are working on
   * @param parentContent    : Map with the content (empty at the beginning) of the parent directory. The content will be updated progressively
   */
  @tailrec
  def createCommitTree(repoPath: String, indexPaths: List[Array[String]], indexContent: Map[String, String], currentPathDepth: Int, parentContent: Map[String, Array[String]] = Map()): String = {

    /** If we have treated all the directories/files present in INDEX */
    if (currentPathDepth == 0) {

      // Create Commit Tree object in .sgit/objects
      val commitTreeContent = parentContent("") mkString "\n"
      ObjectUtil.addSGitObject(repoPath, commitTreeContent)
    }
    else {
      /** Get the paths of INDEX having the current size in a List of String and with no doublons */
      val paths = indexPaths.filter(path => path.length == currentPathDepth).map(path => path mkString File.separator).distinct

      /** Create blobs and then trees of the parent */
      val parentContentWithBlobs = createObjectOfParent(repoPath, "blob", paths, indexContent, parentContent)
      val parentContentWithBlobsTrees = createObjectOfParent(repoPath, "tree", paths, indexContent, parentContentWithBlobs)


      /** We do the same with the paths with a shorter depth than the current */
      // Remove elements treated (having the current depth). In this way, they will not be treated in next recursive call
      val pathsWithoutLastElement = indexPaths.map(path => removeElementsOfDepth(path, currentPathDepth))
      createCommitTree(repoPath, pathsWithoutLastElement, indexContent, currentPathDepth - 1, parentContentWithBlobsTrees)
    }
  }

  /**
   * Create objects and update the parent with those objects
   *
   * @param repoPath      : path of the sgit repository
   * @param objectType    : determines what type of object wa have tp create, "blob" or "tree"
   * @param objectPaths   : list of all the paths of the objects we have to create
   * @param indexContent  : Map containing the lines of .sgit/INDEX with 'paths as keys' and 'hashes as values'
   * @param parentContent : Map with the content of the parent directory. This content will be updated with the objects created
   * @return the content of the parent updated with the objects created from objectPaths
   */
  def createObjectOfParent(repoPath: String, objectType: String, objectPaths: List[String], indexContent: Map[String, String], parentContent: Map[String, Array[String]]): Map[String, Array[String]] = {

    objectType match {
      case "blob" => {
        // From the paths, we keep only the files in order to create the Blobs
        val files = objectPaths.filter(path => indexContent.contains(path))
        // Get the parent of each file
        var filesParents = files.map(path => path.split(File.separator).slice(0, path.split(File.separator).length - 1) mkString File.separator)
        // Create the blobs as a string in the format "blob hash fileName"
        var blobs = files.map(filePath => "blob " + indexContent(filePath) + " " + filePath.split(File.separator).last)
        // Update the content of parent Tree with the blobs created
        updateParentContent(parentContent, filesParents, blobs)
      }

      case "tree" => {
        // From the paths obtained, we keep only the folders in order to create the Trees
        val folders = objectPaths.filter(path => !indexContent.contains(path))
        // Get the parent of each folder
        var foldersParents = folders.map(path => path.split(File.separator).slice(0, path.split(File.separator).length - 1) mkString File.separator)
        // Creates the trees as a string in the format "tree hash folderName"
        var trees = folders.map(folderPath => "tree " + ObjectUtil.hash(parentContent(folderPath) mkString "\n") + " " + folderPath.split(File.separator).last)
        // Update the content of parent Tree with the trees created
        val parentWithTrees = updateParentContent(parentContent, foldersParents, trees)

        // Create the Trees objects in .sgit/objects
        val treesContent = folders.map(treePath => parentWithTrees(treePath) mkString "\n")
        treesContent.foreach(content => ObjectUtil.addSGitObject(repoPath, content))
        parentWithTrees
      }
    }
  }

  /**
   *
   * @param parentContent           : Map with the content of the parent directory. This content will be updated with the new content
   * @param parentPathsOfNewContent :
   * @param newContentLines         : lines to add in parent
   * @return the parent content updated
   */
  @tailrec
  def updateParentContent(parentContent: Map[String, Array[String]], parentPathsOfNewContent: List[String], newContentLines: List[String]): Map[String, Array[String]] = {
    parentPathsOfNewContent match {
      case Nil => parentContent
      case path :: tail => {
        val parentUpdated = updateParentWithOneContent(parentContent, path, newContentLines.head)
        updateParentContent(parentUpdated, tail, newContentLines.tail)
      }
    }
  }

  /**
   *
   * @param parentContent          : Map with the content of the parent directory. This content will be updated with the new content
   * @param pathParentOfNewContent : the panret path of the new content we want to add in parentContent
   * @param newContent             : the new content we want to add
   * @return Add the newContent in the parentContent
   */
  def updateParentWithOneContent(parentContent: Map[String, Array[String]], pathParentOfNewContent: String, newContent: String): Map[String, Array[String]] = {

    if (parentContent.contains(pathParentOfNewContent)) {
      val currentContentForPath = parentContent(pathParentOfNewContent)
      val updatedContentForPath = currentContentForPath.patch(0, Array(newContent), 0)
      parentContent + (pathParentOfNewContent -> updatedContentForPath)
    } else {
      parentContent + (pathParentOfNewContent -> Array(newContent))
    }
  }

  /**
   * Return the path without elements having currentDepth
   *
   * @param path         : path split by elements in Array
   * @param currentDepth : depth used to treat elements in paths having it
   */
  def removeElementsOfDepth(path: Array[String], currentDepth: Int): Array[String] = {
    if (path.length == currentDepth) path.slice(0, currentDepth - 1)
    else path
  }
}