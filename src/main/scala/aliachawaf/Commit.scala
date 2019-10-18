package aliachawaf

import java.io.File

import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, IndexUtil, ObjectUtil, ResultUtil}

import scala.annotation.tailrec

object Commit {

  /**
   * @param repoPath : path of the sgit repository
   * @return
   */
  def commit(repoPath: String, commitMsg: String): String = {

    if (Repository.hasIndexFile(repoPath)) {

      /*_______________ I/O PART _______________*/
      val indexContent = IndexUtil.getIndexAsMap(repoPath)

      /*_______________ PURE PART _______________*/
      val commitTree = createCommitTree(repoPath, indexContent)
      val commitTreeHash = ObjectUtil.hash(commitTree("") mkString "\n")

      /*_______________ I/O PART _______________*/
      ObjectUtil.createTreeObjects(commitTree.values.toList, repoPath)
      addCommitInObjects(repoPath, commitTreeHash, commitMsg)
    }

    else ResultUtil.nothingToCommit(repoPath)
  }


  /**
   * @return The hash of the Commit Tree created recursively
   * @param repoPath     : path of the SGit repository
   * @param indexContent : Map containing the lines of INDEX with 'paths as keys' and 'hashes as values'
   */
  def createCommitTree(repoPath: String, indexContent: Map[String, String]): Map[String, List[String]] = {



    @tailrec
    def loop(currentPathDepth: Int, parentContent: Map[String, List[String]], currentIndexPaths: List[List[String]]): Map[String, List[String]] = {

      /** If we have treated all the directories/files present in INDEX */
      if (currentPathDepth == 0) parentContent

      else {
        /** Get the paths of INDEX having the current size in a List of String and with no doublons */
        val paths = currentIndexPaths.filter(path => path.length == currentPathDepth).map(path => path mkString File.separator).distinct

        /** Create blobs and then trees of the parent */
        val parentContentWithBlobs = createObjectOfParent(repoPath, "blob", paths, indexContent, parentContent)
        val parentContentWithBlobsTrees = createObjectOfParent(repoPath, "tree", paths, indexContent, parentContentWithBlobs)

        /** We do the same with the paths with a shorter depth than the current */
        // Remove elements treated (having the current depth). In this way, they will not be treated in next recursive call
        val pathsWithoutLastElement = currentIndexPaths.map(path => removeElementsOfDepth(path, currentPathDepth))
        loop(currentPathDepth - 1, parentContentWithBlobsTrees, pathsWithoutLastElement)
      }
    }

    // Keep only paths form index lines and split by path elements
    val indexPaths = indexContent.keys.toList.map(l => l.split(File.separator).toList)
    val maxDepth = indexPaths.sortBy(path => path.length).reverse.head.length
    loop(maxDepth, Map(), indexPaths)
  }


  /**
   *
   * @param repoPath
   * @param commitTreeHash : hash of the new tree created with the commit
   * @param commitMsg      : message of the commit
   * @return the msg to display to the user after sgit commit
   */
  def addCommitInObjects(repoPath: String, commitTreeHash: String, commitMsg: String) = {

    /** Commit content :
     *    - tree hash
     *    - parent hash
     *    - message messageContent
     */

    val currentBranchName = BranchUtil.getCurrentBranchName(repoPath)

    // If there is no differences between current commit and previous commit, we don't have to create the new commit
    if (commitTreeHash != CommitUtil.getLastCommitTree(repoPath).getOrElse("")) {
      // Get parent commit
      val currentBranchPath = repoPath + File.separator + ".sgit" + File.separator + BranchUtil.getCurrentBranch(repoPath)

      // If not first commit for this branch
      if (new File(currentBranchPath).exists()) {
        val parentCommit = FileUtil.getFileContent(currentBranchPath) mkString "\n"
        val commitContent = "tree " + commitTreeHash + "\n" + "parent " + parentCommit + "\n" + "message " + commitMsg
        val commitHash = ObjectUtil.addSGitObject(repoPath, commitContent)
        // Update current branch reference
        FileUtil.createNewFile(currentBranchPath, commitHash)

        ResultUtil.commitResult(currentBranchName, commitHash, commitMsg)

      }
      // If it is the first commit of the branch, then the commit has no parent
      else {
        val commitContent = "tree " + commitTreeHash + "\n" + "message " + commitMsg
        val commitHash = ObjectUtil.addSGitObject(repoPath, commitContent)
        // Update current branch reference
        FileUtil.createNewFile(currentBranchPath, commitHash)
        ResultUtil.commitResult(currentBranchName, commitHash, commitMsg)
      }
    } else {
      ResultUtil.sameCommitResult(currentBranchName)
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
  def createObjectOfParent(repoPath: String, objectType: String, objectPaths: List[String], indexContent: Map[String, String], parentContent: Map[String, List[String]]): Map[String, List[String]] = {

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
        //treesContent.foreach(content => ObjectUtil.addSGitObject(repoPath, content))
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
  def updateParentContent(parentContent: Map[String, List[String]], parentPathsOfNewContent: List[String], newContentLines: List[String]): Map[String, List[String]] = {
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
  def updateParentWithOneContent(parentContent: Map[String, List[String]], pathParentOfNewContent: String, newContent: String): Map[String, List[String]] = {

    if (parentContent.contains(pathParentOfNewContent)) {
      val currentContentForPath = parentContent(pathParentOfNewContent)
      val updatedContentForPath = currentContentForPath.patch(0, List(newContent), 0)
      parentContent + (pathParentOfNewContent -> updatedContentForPath)
    } else {
      parentContent + (pathParentOfNewContent -> List(newContent))
    }
  }

  /**
   * Return the path without elements having currentDepth
   *
   * @param path         : path split by elements in Array
   * @param currentDepth : depth used to treat elements in paths having it
   */
  def removeElementsOfDepth(path: List[String], currentDepth: Int): List[String] = {
    if (path.length == currentDepth) path.slice(0, currentDepth - 1)
    else path
  }


}