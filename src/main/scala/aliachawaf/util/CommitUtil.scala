package aliachawaf.util

import java.io.File
import java.nio.file.Path

import scala.annotation.tailrec

object CommitUtil {

  /**
   *
   * @param repoPath
   * @param currentBranch : name of the current branch in the format "branches/name"
   * @return the hash of the last commit done, else None if there is no commit done
   */
  def getLastCommit(repoPath: String, currentBranch: String): Option[String] = {
    val branchPath = repoPath + File.separator + ".sgit" + File.separator + currentBranch
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
      val lastCommitContent = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + lastCommit.get)
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

    val treePath = repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + treeHash
    val treeContent = FileUtil.getFileContent(treePath)

    // If we are at the end of the path, then it is the file name
    if (filePath.length == 1) {

      // Keep only blobs of the tree to check if the file is contained by the tree
      val allBlobs = treeContent.filter(_.split(" ")(0) == "blob")
      val blobCorresponding = allBlobs.filter(_.split(" ")(2) == filePath.head)

      blobCorresponding match {
        case Nil => None
        case blob :: Nil => Some(blob.split(" ")(1))
      }
    }
    else {
      // Keep only trees of the tree to check if the current folder is contained
      val allTrees = treeContent.filter(_.split(" ")(0) == "tree")
      val treeCorresponding = allTrees.filter(_.split(" ")(2) == filePath.head)

      treeCorresponding match {
        case Nil => None
        case tree :: tail => getBlobHashCommitted(repoPath, tail, tree.split(" ")(1))
      }
    }
  }

}
