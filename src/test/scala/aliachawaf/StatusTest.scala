package aliachawaf

import java.io.File
import java.io.File.separator
import java.nio.file.Paths

import aliachawaf.util.{FileUtil, IndexUtil}
import aliachawaf.Status._
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class StatusTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    Repository.initialize(currentDir)

    val repoPath = Repository.getRepoPath(currentDir).get
    val testDir = repoPath + separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + separator + "testFile1", "Hello, world!")
    FileUtil.createNewFile(testDir + separator + "testFile2", "Bonjour tout le monde!")

    // For testing relative paths, we create a subfolder
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"
    new File(testDir2).mkdir()

    Index.add(Seq("testDir/testFile1"), repoPath)
  }

  // We delete the sgit repository created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get
    Directory(new File(repoPath + separator + ".sgit")).deleteRecursively()
    Directory(new File(repoPath + separator + "testDir")).deleteRecursively()
  }

  "The status command" should "get untracked files (relative paths)" in {

    val repoPath = Repository.getRepoPath(System.getProperty("user.dir")).get
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val files = get_Untracked(testDir2, repoPath)

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    val file2AbsolutePath = repoPath + separator + "testDir" + separator + "testFile2"
    val file2RelativePath = Paths.get(testDir2).relativize(Paths.get(file2AbsolutePath)).toString

    assert(!files.contains(file1RelativePath))
    assert(files.contains(file2RelativePath))
  }

  it should "get tracked and modified files but not added after edit (relative paths)" in {

    val repoPath = Repository.getRepoPath(System.getProperty("user.dir")).get
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    val files = get_Tracked_Modified_NotAdded(testDir2, repoPath)
    assert(files.isEmpty)

    // Edit file
    FileUtil.writeFile(new File(repoPath + separator + "testDir" + separator + "testFile1"), "Goodbye".getBytes.toList, append = true)
    val filesAfterEdit = get_Tracked_Modified_NotAdded(testDir2, repoPath)

    assert(filesAfterEdit.length == 1)
    assert(filesAfterEdit.head == file1RelativePath)

    // Add file
    Index.add(Seq("testDir/testFile1"), repoPath)
    val filesAfterAdd = get_Tracked_Modified_NotAdded(testDir2, repoPath)
    assert(filesAfterAdd.isEmpty)
  }

  it should "get tracked and never committed files (relative paths)" in {

    val repoPath = Repository.getRepoPath(System.getProperty("user.dir")).get
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    val files = get_Tracked_NeverCommitted(testDir2, repoPath)
    assert(files.length == 1)
    assert(files.head == file1RelativePath)

    Commit.commit(repoPath, "commit testFile1")
    val filesAfterCommit = get_Tracked_NeverCommitted(testDir2, repoPath)
    assert(filesAfterCommit.isEmpty)
  }

  it should "get tracked modified and committed files (relative paths)" in {

    val repoPath = Repository.getRepoPath(System.getProperty("user.dir")).get
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    // No Commit
    val files = get_Tracked_Committed_Modified(testDir2, repoPath)
    assert(files.isEmpty)

    // With Commit
    Commit.commit(repoPath, "first commit")

    /* edit file and add it */
    val testFile1Path = repoPath + separator + "testDir" + separator + "testFile1"
    val testFile2Path = repoPath + separator + "testDir" + separator + "testFile2"

    FileUtil.writeFile(new File(testFile1Path), "Goodbye".getBytes.toList, append = true)
    Index.add(Seq(testFile1Path, testFile2Path), repoPath)

    val filesAfterCommitEditAdd = get_Tracked_Committed_Modified(testDir2, repoPath)
    assert(filesAfterCommitEditAdd.length == 1)
    assert(filesAfterCommitEditAdd.head == file1RelativePath)
  }

  it should "get all the tracked files but not present in working tree (deleted)" in {

    val repoPath = Repository.getRepoPath(System.getProperty("user.dir")).get
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    new File(file1AbsolutePath).delete()

    val files = get_Deleted_NotAdded(testDir2, repoPath)
    assert(files.length == 1)
    assert(files.head == file1RelativePath)
  }


}
