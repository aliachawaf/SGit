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
   * @return the given commit tree as a Map(filePath, hash)
   */
  def getCommitAsMap(repoPath: String, treeHash: Option[String]): Option[Map[String, List[String]]] = {

    /**
     * @param contentTree : list of the lines contained in the current tree we are treating
     * @param parentPath  : path of parent tree of the tree we are treating
     * @param commitMap   : the final map updated progressively with the blobs of the tree
     * @return
     */
    def loop(contentTree: List[String], parentPath: String, commitMap: Map[String, List[String]]): Map[String, List[String]] = {

      contentTree match {
        case Nil => commitMap

        case head :: tail =>

          if (head.split(" ")(0) == "blob") {

            val blobHash = head.split(" ")(1)
            val blobContent = ObjectUtil.getObjectContent(repoPath, blobHash)

            val blobName = parentPath + head.split(" ")(2)

            val newCommitMap = commitMap + (blobName -> blobContent)
            loop(tail, parentPath, newCommitMap)
          }
          else {
            val subTreeHash = head.split(" ")(1)
            val subTreeName = parentPath + head.split(" ")(2)
            val contentSubTree = FileUtil.getFileContent(repoPath + separator + ".sgit" + separator + "objects" + separator + subTreeHash)
            val newCommitMap = loop(contentSubTree, parentPath + separator + subTreeName, commitMap)

            loop(tail, parentPath, newCommitMap)
          }
      }
    }

    if (treeHash.isEmpty) None
    else Some(loop(ObjectUtil.getObjectContent(repoPath, treeHash.get), "", Map().withDefaultValue(List())))
  }


  def getCommitMessage(commitContent: List[String]) : String = {

    println(commitContent)
    // If is first commit (no parent)
    if (commitContent.length == 2) commitContent(1).split(" ").tail mkString " "
    else commitContent(2).split(" ").tail mkString " "
  }

  def getCommitParent(commitContent: List[String]) : Option[String] = {

    // If is first commit (no parent)
    if (commitContent.length == 2) None
    else Some(commitContent(1).split(" ")(1))
  }
}
