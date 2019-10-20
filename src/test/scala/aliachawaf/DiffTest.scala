package aliachawaf

import java.io.File
import java.io.File.separator

import aliachawaf.command.{Diff, Index, Init}
import aliachawaf.util.{FileUtil, RepoUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class DiffTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    Init.initialize(currentDir)

    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + File.separator + "testFile1", "Alia\nLire\nInes\nAlia")
    FileUtil.createNewFile(testDir + File.separator + "testFile2", "Bien\nAlia\nLire\nInes\nRemi\nEniram")

    Index.add(Seq("testDir/testFile1"), repoPath)
  }

  // We delete the sgit repository created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get
    Directory(new File(repoPath + File.separator + ".sgit")).deleteRecursively()
    Directory(new File(repoPath + File.separator + "testDir")).deleteRecursively()
  }

  "Diff command" should "get lines differences between two versions of a file" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get

    val newFileContent = FileUtil.getFileContent(repoPath + separator + "testDir" + separator + "testFile2")
    val oldFileContent = FileUtil.getFileContent(repoPath + separator + "testDir" + separator + "testFile1")

    val matrix = Diff.getMatrixOfComparison(newFileContent, oldFileContent)

    val listDiff = Diff.getDiffLines(matrix, newFileContent.length, oldFileContent.length)
    val expectedListDiff = List(("++",1), ("++",5), ("++",6), ("--",4))

    assert(listDiff == expectedListDiff)

  }

}
