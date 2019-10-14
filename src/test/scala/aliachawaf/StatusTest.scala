package aliachawaf

import java.io.File
import java.io.File.separator

import aliachawaf.util.FileUtil
import aliachawaf.Status._
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class StatusTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    Repository.initialize(currentDir)

    val repoPath = Repository.getRepoPath(currentDir).get
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + File.separator + "testFile1", "Hello, world!")
    FileUtil.createNewFile(testDir + File.separator + "testFile2", "Bonjour tout le monde!")

    Index.add(Seq("testDir/testFile1"), repoPath)
  }

  // We delete the sgit repository created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    Directory(new File(repoPath + File.separator + ".sgit")).deleteRecursively()
    Directory(new File(repoPath + File.separator + "testDir")).deleteRecursively()
  }

  "The status command" should "get untracked files" in {

    val currentDir = System.getProperty("user.dir")

    val files = get_Untracked(currentDir)

    assert(!files.contains("testDir" + separator + "testFile1"))
    assert(files.contains("testDir" + separator + "testFile2"))
  }

  it should "get tracked and modified files but not added" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get

    val files = get_Tracked_Modified_NotAdded(currentDir)
   // assert(!files.contains("testDir" + separator + "testFile1"))

    FileUtil.writeFile(new File(repoPath + File.separator + "testDir" + separator + "testFile1"), "Goodbye".getBytes.toList, append = true)
    val filesAfterEdit = get_Tracked_Modified_NotAdded(currentDir)
  //  assert(files.contains("testDir" + separator + "testFile1"))

    Index.add(Seq("testDir/testFile1"), repoPath)
    val filesAfterAdd = get_Tracked_Modified_NotAdded(currentDir)
  //  assert(!files.contains("testDir" + separator + "testFile1"))
  }


}
