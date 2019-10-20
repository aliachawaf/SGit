package aliachawaf.util

import java.io.File
import java.io.File.separator

object CommitUtil {

  /**
   *
   * @param repoPath      : path of the SGit repository
   * @param currentBranch : name of the current branch in the format "branches/name"
   * @return the hash of the last commit done by the current branch, else None if there is no commit yet
   */
  def getLastCommit(repoPath: String, currentBranch: String): Option[String] = {
    val branchPath = repoPath + separator + ".sgit" + separator + currentBranch
    if (new File(branchPath).exists())
      Some(FileUtil.getFileContent(branchPath) mkString "\n")
    else None
  }

  /**
   * @param repoPath : path of the SGit repository
   * @return the hash of the tree of the last commit for the given SGit repository, else None if there is no commit yet
   */
  def getLastCommitTree(repoPath: String): Option[String] = {
    val currentBranch = BranchUtil.getCurrentBranch(repoPath)
    val lastCommit = getLastCommit(repoPath, currentBranch)

    if (lastCommit.isDefined) {
      val lastCommitContent = ObjectUtil.getObjectContent(repoPath, lastCommit.get)
      Some(lastCommitContent(0).split(" ")(1))
    }
    else None
  }


  /**
   *
   * @param repoPath : path of the SGit repository
   * @param treeHash : hash of the commit tree we want to have as a Map, None if there is no tree
   * @return the given commit tree as a Map(filePath, content), else return None if there is no tree
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

            val blobName = parentPath + separator + head.split(" ")(2)

            val newCommitMap = commitMap + (blobName -> blobContent)
            loop(tail, parentPath, newCommitMap)
          }
          else {
            val subTreeHash = head.split(" ")(1)
            val subTreeName = head.split(" ")(2)
            val contentSubTree = ObjectUtil.getObjectContent(repoPath, subTreeHash)

            if (parentPath.isEmpty) {
              val newCommitMap = loop(contentSubTree, subTreeName, commitMap)
              loop(tail, parentPath, newCommitMap)
            } else {
              val newCommitMap = loop(contentSubTree, parentPath + separator + subTreeName, commitMap)
              loop(tail, parentPath, newCommitMap)
            }
          }
      }
    }

    if (treeHash.isEmpty) None
    else Some(loop(ObjectUtil.getObjectContent(repoPath, treeHash.get), "", Map().withDefaultValue(List())))
  }


  /**
   *
   * @param commitContent
   * @return the message of the given commit
   */
  def getCommitMessage(commitContent: List[String]): String = {

    // If is first commit (no parent)
    if (commitContent.length == 2) commitContent(1).split(" ").tail mkString " "
    else commitContent(2).split(" ").tail mkString " "
  }

  /**
   *
   * @param commitContent
   * @return the parent of the given commit, else None if it is the first commit (no parent)
   */
  def getCommitParent(commitContent: List[String]): Option[String] = {

    if (commitContent.length == 2) None
    else Some(commitContent(1).split(" ")(1))
  }
}