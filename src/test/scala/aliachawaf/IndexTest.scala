package aliachawaf

import java.io.File

import aliachawaf.Index.add
import aliachawaf.util.{FileUtil, IndexUtil, ObjectUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class IndexTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val dir = System.getProperty("user.dir") + File.separator + "dir"
    new File(dir).mkdir()
    Repository.initialize(dir)

    val repoPath = Repository.getRepoPath(dir).get
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + File.separator + "testFile1", "Hello, world!")
    FileUtil.createNewFile(testDir + File.separator + "testFile2", "Bonjour tout le monde!")
  }

  // We delete the sgit repository and files created after each test
  override def afterEach(): Unit = {
    val dir = System.getProperty("user.dir") + File.separator + "dir"
    Directory(new File(dir)).deleteRecursively()
  }

  "An Index" should "create index file for first add" in {
    val repoPath = System.getProperty("user.dir") + File.separator + "dir"

    assert(!IndexUtil.indexFileExists(repoPath))
    add(Seq("testFile1"), repoPath)
    assert(IndexUtil.indexFileExists(repoPath))
  }

  it should "create a blob in .sgit/objects when a file is added" in {

    val repoPath = System.getProperty("user.dir") + File.separator + "dir"
    add(Seq(repoPath + File.separator + "testDir" + File.separator + "testFile1"), repoPath)

    val filePath = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val content = FileUtil.getFileContent(filePath) mkString "\n"
    val hash = ObjectUtil.hash(content)

    val blobPath = repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + hash
    assert(new File(blobPath).exists())

    val blobContent = FileUtil.getFileContent(blobPath) mkString "\n"
    assert(blobContent == content)
  }

  it should "add given files as blob in .sgit/INDEX file" in {
    val repoPath = System.getProperty("user.dir") + File.separator + "dir"
    add(Seq(repoPath + File.separator + "testDir" + File.separator + "testFile1"), repoPath)

    val filePath = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val content = FileUtil.getFileContent(filePath) mkString "\n"
    val hash = ObjectUtil.hash(content)
    val indexLine = IndexUtil.getIndexContent(repoPath).head

    assert(indexLine == (hash + " " + "testDir/testFile1"))
  }

  it should "be able to add many files given in add command" in {

    val repoPath = System.getProperty("user.dir") + File.separator + "dir"
    val file1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val file2 = repoPath + File.separator + "testDir" + File.separator + "testFile2"

    add(Seq(file1, file2), repoPath)

    val pathFile1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val pathFile2 = repoPath + File.separator + "testDir" + File.separator + "testFile2"
    val contentFile1 = FileUtil.getFileContent(pathFile1) mkString "\n"
    val contentFile2 = FileUtil.getFileContent(pathFile2) mkString "\n"
    val hashFile1 = ObjectUtil.hash(contentFile1)
    val hashFile2 = ObjectUtil.hash(contentFile2)

    val indexLines = IndexUtil.getIndexContent(repoPath)

    assert(indexLines.head == (hashFile1 + " " + "testDir/testFile1"))
    assert(indexLines(1) == (hashFile2 + " " + "testDir/testFile2"))

  }

  it should "update index with the new hash for modified files already indexed" in {

    val repoPath = System.getProperty("user.dir") + File.separator + "dir"
    val file1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    add(Seq(file1), repoPath)

    // Edit testFile1 and add it
    val pathFile1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    FileUtil.writeFile(new File(pathFile1), ("Second Hello, World!").getBytes.toList, append = true)
    add(Seq(file1), repoPath)

    val newContent = FileUtil.getFileContent(pathFile1) mkString "\n"
    val newHash = ObjectUtil.hash(newContent)

    val indexLines = IndexUtil.getIndexContent(repoPath)
    assert(indexLines(0) == (newHash + " " + "testDir/testFile1"))
  }

  it should "not add a line in INDEX if arguments do not match any files" in {
    val repoPath = System.getProperty("user.dir") + File.separator + "dir"
    add(Seq("anything"), repoPath)
    val indexLines = IndexUtil.getIndexContent(repoPath)
    assert(indexLines.isEmpty)
  }

  it should "remove files deleted from .sgit/INDEX" in {
    val repoPath = System.getProperty("user.dir") + File.separator + "dir"
    val file1 = repoPath + File.separator + "testDir" + File.separator + "testFile1"
    val file2 = repoPath + File.separator + "testDir" + File.separator + "testFile2"
    add(Seq(file1, file2), repoPath)

    new File(file1).delete()
    add(Seq(file1), repoPath)

    val indexLines = IndexUtil.getIndexContent(repoPath)
    val indexPaths = IndexUtil.getIndexPaths(repoPath)
    assert(indexLines.length == 1)
    assert(indexPaths.head != file1)
  }
}