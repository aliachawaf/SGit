package aliachawaf.util

import java.io.File
import java.math.BigInteger
import java.security.MessageDigest

object ObjectUtil {

  /**
   * @param s : a String, in general the content of the Object we want to hash
   * @return : the hash of s
   */
  def hash(s: String): String = {
    val md = MessageDigest.getInstance("SHA-1")
    val digest = md.digest(s.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }

  /**
   * Create the object file in .sgit/objects
   *
   * @param repoPath      : path of the SGit repository
   * @param objectContent : content of the SGit object (tree or blob) to put in the file and hash for the file name
   * @return : the hash of the object created
   */
  def addSGitObject(repoPath: String, objectContent: String): String = {
    val objectHash = hash(objectContent)
    val objectPath = repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + objectHash
    FileUtil.createNewFile(objectPath, objectContent)
    objectHash
  }


  def createTreeObjects(trees: List[List[String]], repoPath: String) : Unit = {
    trees match {
      case Nil => {}
      case head :: tail => {
        ObjectUtil.addSGitObject(repoPath, head mkString "\n")
        createTreeObjects(tail, repoPath)
      }
    }
  }

  def getObjectContent(repoPath: String, hashObject: String) : List[String] ={
    val objectPath = repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + hashObject
    FileUtil.getFileContent(objectPath)
  }
}
