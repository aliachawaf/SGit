package aliachawaf

import java.io.File

import aliachawaf.util.{FileUtil, ObjectUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import aliachawaf.Index.add
import scala.reflect.io.Directory

class IndexTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    Repository.initialize(currentDir)

    val repoPath = Repository.getRepoPath(currentDir).get
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + File.separator + "testFile1", "Hello, world!")
    FileUtil.createNewFile(testDir + File.separator + "testFile2", "Bonjour tout le monde!")
  }

  // We delete the sgit repository and files created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    Directory(new File(repoPath + File.separator + ".sgit")).deleteRecursively()
    Directory(new File(repoPath + File.separator + "testDir")).deleteRecursively()
  }

  "An Index" should "create index file for first add" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get

    assert(!Repository.hasIndexFile(repoPath))
    add(Seq("testFile1"), repoPath)
    assert(Repository.hasIndexFile(repoPath))
  }

  it should "create a blob in .sgit/objects when a file is added" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    add(Seq("testDir/testFile1"), repoPath)

    val filePath = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val content = FileUtil.getFileContent(filePath) mkString "\n"
    val hash = ObjectUtil.hash(content)

    val blobPath = repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + hash
    assert(new File(blobPath).exists())

    val blobContent = FileUtil.getFileContent(blobPath) mkString "\n"
    assert(blobContent == content)
  }

  it should "add given files as blob in .sgit/INDEX file" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    add(Seq("testDir/testFile1"), repoPath)

    val filePath = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val content = FileUtil.getFileContent(filePath) mkString "\n"
    val hash = ObjectUtil.hash(content)
    val indexLine = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")(0)

    assert(indexLine == (hash + " " + "testDir/testFile1"))
  }

  it should "be able to add many files given in add command" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    add(Seq("testDir/testFile1", "testDir/testFile2"), repoPath)

    val pathFile1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val pathFile2 = repoPath + File.separator + "testDir" + File.separator + "testFile2"
    val contentFile1 = FileUtil.getFileContent(pathFile1) mkString "\n"
    val contentFile2 = FileUtil.getFileContent(pathFile2) mkString "\n"
    val hashFile1 = ObjectUtil.hash(contentFile1)
    val hashFile2 = ObjectUtil.hash(contentFile2)

    val indexLines = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")

    assert(indexLines(0) == (hashFile1 + " " + "testDir/testFile1"))
    assert(indexLines(1) == (hashFile2 + " " + "testDir/testFile2"))

  }
/*
  it should "add all files when '.' is used with sgit add command" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    Index.add(Seq("."), repoPath)

    val pathFile1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val pathFile2 = repoPath + File.separator + "testDir" + File.separator + "testFile2"
    val contentFile1 = FileUtil.getFileContent(pathFile1) mkString "\n"
    val contentFile2 = FileUtil.getFileContent(pathFile2) mkString "\n"
    val hashFile1 = ObjectUtil.hash(contentFile1)
    val hashFile2 = ObjectUtil.hash(contentFile2)

    val indexLines = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")

    assert(indexLines.length > 1)
  }*/

  it should "update index with the new hash for modified files already indexed" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    add(Seq("testDir/testFile1"), repoPath)

    // Edit testFile1 and add it
    val pathFile1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    FileUtil.writeFile(new File(pathFile1), ("New Hello, World!").getBytes.toList, append = false)
    add(Seq("testDir/testFile1"), repoPath)

    val newContent = FileUtil.getFileContent(pathFile1) mkString "\n"
    val newHash = ObjectUtil.hash(newContent)

    val indexLines = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")

    assert(indexLines(0) == (newHash + " " + "testDir/testFile1"))
  }

  it should "not add a line in INDEX if arguments do not match any files" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    add(Seq("anything"), repoPath)
    val indexLines = FileUtil.getFileContent(repoPath + File.separator + ".sgit" + File.separator + "INDEX")
    assert(indexLines.isEmpty)
  }
}