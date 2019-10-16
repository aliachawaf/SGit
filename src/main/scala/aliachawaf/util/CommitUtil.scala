package aliachawaf.util

import java.io.File
import java.io.File.separator

import scala.annotation.tailrec

object CommitUtil {

  /**
   *
   * @param repoPath
   * @param currentBranch : name of the current branch in the format "branches/name"
   * @return the hash of the last commit done, else None if there is no commit done
   */
  def getLastCommit(repoPath: String, currentBranch: String): Option[String] = {
    val branchPath = repoPath + separator + ".sgit" + separator + currentBranch
    if (new File(branchPath).exists()) Some(FileUtil.getFileContent(branchPath) mkString "\n")
    else None
  }

  /**
   * @param repoPath
   * @return the hash of the tree of the last commit, else None if there is no commit done
   */
  def getLastCommitTree(repoPath: String): Option[String] = {
    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = getLastCommit(repoPath, currentBranch)

    if (lastCommit.isDefined) {
      val lastCommitContent = FileUtil.getFileContent(repoPath + separator + ".sgit" + separator + "objects" + separator + lastCommit.get)
      Some(lastCommitContent(0).split(" ")(1))
    }
    else None
  }

  /**
   *
   * @param repoPath : path of the sgit repository
   * @param filePath : path of the file we want to check if is committed
   * @param treeHash : the commit tree at the beginning, and then subtrees
   * @return the hash of the file already committed before, else if the file was never committed returns None
   */
  @scala.annotation.tailrec
  def getBlobHashCommitted(repoPath: String, filePath: List[String], treeHash: String): Option[String] = {

    val treePath = repoPath + separator + ".sgit" + separator + "objects" + separator + treeHash
    val treeContent = FileUtil.getFileContent(treePath)

    // If we are at the end of the path, then it is the file name
    if (filePath.length == 1) {

      // Keep only blobs of the tree to check if the file is contained by the tree
      val allBlobs = treeContent.filter(_.split(" ")(0) == "blob")
      val blobCorresponding = allBlobs.filter(_.split(" ")(2) == filePath.head)

      if (blobCorresponding.isEmpty) None
      else Some(blobCorresponding.head.split(" ")(1))

    }
    else {
      // Keep only trees of the tree to check if the current folder is contained
      val allTrees = treeContent.filter(_.split(" ")(0) == "tree")
      val treeCorresponding = allTrees.filter(_.split(" ")(2) == filePath.head)

      if (treeCorresponding.isEmpty) None
      else {
        getBlobHashCommitted(repoPath, filePath.tail, treeCorresponding.head.split(" ")(1))
      }
    }
  }


  /**
   *
   * @param repoPath : path of the sgit repository
   * @param treeHash : hash of the commit tree we want to have as a Map
   * @return the given commit tree as a Map(filePath, hash)
   */
  def getCommitAsMap(repoPath: String, treeHash: String): Map[String, String] = {

    /**
     * @param contentTree : list of the lines contained in the current tree we are treating
     * @param parentPath  : path of parent tree of the tree we are treating
     * @param commitMap   : the final map updated progressively with the blobs of the tree
     * @return
     */
    def loop(contentTree: List[String], parentPath: String, commitMap: Map[String, String]): Map[String, String] = {

      contentTree match {
        case Nil => commitMap

        case head :: tail =>

          if (head.split(" ")(0) == "blob") {

            val blobHash = head.split(" ")(1)
            val blobName = parentPath + separator + head.split(" ")(2)
            val newCommitMap = commitMap + (blobName -> blobHash)
            loop(tail, parentPath, newCommitMap)

          }
          else {
            val subTreeHash = head.split(" ")(1)
            val subTreeName = parentPath + separator + head.split(" ")(2)
            val contentSubTree = FileUtil.getFileContent(repoPath + separator + ".sgit" + separator + "objects" + separator + subTreeHash)
            val newCommitMap = loop(contentSubTree, parentPath + separator + subTreeName, commitMap)

            loop(tail, parentPath, newCommitMap)
          }
      }
    }

    val commitTreeContent = FileUtil.getFileContent(repoPath + separator + ".sgit" + separator + "objects" + separator + treeHash)

    loop(commitTreeContent, "", Map())
  }


  def getCommitMessage(commitContent: List[String]) : String = {

    // If is first commit (no parent)
    if (commitContent.length == 2) commitContent(1).split(" ")(1)
    else commitContent(2).split(" ")(1)

  }
}
