package aliachawaf

import java.io.File
import java.io.File.separator

import aliachawaf.util.FileUtil
import aliachawaf.Diff._
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

class DiffTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    Repository.initialize(currentDir)

    val repoPath = Repository.getRepoPath(currentDir).get
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + File.separator + "testFile1", "A\nL\nI\nA\n")
    FileUtil.createNewFile(testDir + File.separator + "testFile2", "B\nA\nL\nI\nR\nE")
  }

  "Diff command" should "construct the matrix of the longest common sequence" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get

    val newFileContent = FileUtil.getFileContent(repoPath + separator + "testDir" + separator + "testFile2")
    val oldFileContent = FileUtil.getFileContent(repoPath + separator + "testDir" + separator + "testFile1")

    val matrix = getMatrixOfComparison(newFileContent, oldFileContent)
    val diff = getDiffLines(matrix, newFileContent.length, oldFileContent.length)

    println(matrix)
    println(diff)


  }

}
