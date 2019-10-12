package aliachawaf

import java.io.File
import aliachawaf.util.FileUtil
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import scala.reflect.io.Directory

class RepositoryTest extends FlatSpec with BeforeAndAfterEach {

  // We initialize the sgit repository before each test
  override def beforeEach(): Unit = {
    Repository.initialize(System.getProperty("user.dir"))
  }

  // We delete the sgit repository created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get + File.separator + ".sgit"
    Directory(new File(repoPath)).deleteRecursively()

    val test = new File(currentDir + File.separator + "testDir")
    if (test.exists()) new Directory(test).deleteRecursively()
  }

  "A Repository" should "create the .sgit directory with the right structure after initialization" in {
    assert(new File(".sgit").exists())
    assert(new File(".sgit" + File.separator + "HEAD").exists())
    assert(new File(".sgit" + File.separator + "branches").exists())
    assert(new File(".sgit" + File.separator + "tags").exists())
    assert(new File(".sgit" + File.separator + "objects").exists())
  }

  it should "fill .sgit/HEAD file with the branch master" in {
    val headContent = FileUtil.getFileContent(".sgit" + File.separator + "HEAD") mkString "\n"
    assert(headContent == "branches/master")
  }

  it should "check if a directory is already initialized with .sgit" in {
    val currentDir = System.getProperty("user.dir")
    assert(Repository.isInitialized(currentDir))
    assert(!Repository.isInitialized(""))
  }

  it should "get the sgit repository path of a directory" in {
    // Check current dir
    val currentDir = System.getProperty("user.dir")
    val repoPathCurrentDir = Repository.getRepoPath(currentDir)
    assert(repoPathCurrentDir.isDefined)
    assert(repoPathCurrentDir.get == currentDir)

    // Check subdir
    val testDir = repoPathCurrentDir + File.separator + "testDir"
    new File(testDir).mkdir()
    val repoPathTestDir = Repository.getRepoPath(testDir)
    assert(repoPathTestDir.isDefined)
    assert(repoPathTestDir.get == currentDir)

    // Check dir not in repo
    assert(Repository.getRepoPath("").isEmpty)
  }

  it should "check if a directory is in a sgit repository" in {
    // Check current dir
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    assert(!Repository.isInRepository(""))

    // Check subdir
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    assert(Repository.isInRepository(testDir))

    // Check dir not in repo
    assert(!Repository.isInRepository(""))
  }

  it should "not initialize a directory already initialized" in {
    // Reinitialize current directory
    val currentDir = System.getProperty("user.dir")
    assert(!Repository.initialize(currentDir))

    // Initialize new test directory
    val testDir = Repository.getRepoPath(currentDir).get + File.separator + "testDir"
    new File(testDir).mkdir()
    assert(Repository.initialize(testDir))
    assert(!Repository.initialize(testDir))
  }

  // TODO
  /*
  it should "check if .sgit/INDEX file exists" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    assert(!Repository.hasIndexFile(repoPath))

    FileUtil.createNewFile(repoPath + File.separator + ".sgit" + File.separator + "INDEX", "")
    assert(Repository.hasIndexFile(repoPath))
  }

   */
}