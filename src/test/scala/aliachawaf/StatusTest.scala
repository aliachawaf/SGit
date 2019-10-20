package aliachawaf

import java.io.File
import java.io.File.separator
import java.nio.file.Paths

import aliachawaf.command.{Commit, Index, Init}
import aliachawaf.command.Status._
import aliachawaf.util.{CommitUtil, FileUtil, IndexUtil, RepoUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class StatusTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {

    val repoDir = System.getProperty("user.dir") + separator + "repoDir"
    new File(repoDir).mkdir()
    Init.initialize(repoDir)

    val testDir = repoDir + separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + separator + "testFile1", "Hello, world!")
    FileUtil.createNewFile(testDir + separator + "testFile2", "Bonjour tout le monde!")

    // For testing relative paths, we create a subfolder
    val testDir2 = testDir + separator + "testDir2"
    new File(testDir2).mkdir()

    Index.add(Seq(testDir + separator + "testFile1"), repoDir)
  }

  // We delete the sgit repository created after each test
  override def afterEach(): Unit = {
    val repoDir = System.getProperty("user.dir") + separator + "repoDir"
    Directory(new File(repoDir)).deleteRecursively()
  }

  "The status command" should "get untracked files (relative paths)" in {

    val repoPath = System.getProperty("user.dir") + separator + "repoDir"
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val allRepoFiles = getAllRepoFiles(repoPath)
    val indexMap = IndexUtil.getIndexAsMap(repoPath)

    val file2AbsolutePath = repoPath + separator + "testDir" + separator + "testFile2"
    val file2RelativePath = Paths.get(testDir2).relativize(Paths.get(file2AbsolutePath)).toString

    val files = get_Untracked(allRepoFiles, indexMap, testDir2, repoPath)
    assert(files.length == 1)
    assert(files(0) == file2RelativePath)
  }


  it should "get tracked and modified files but not added after edit (relative paths)" in {

    val repoPath = System.getProperty("user.dir") + separator + "repoDir"
    val testDir = repoPath + separator + "testDir"
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    val allRepoFiles1 = getAllRepoFiles(repoPath)
    val indexMap1 = IndexUtil.getIndexAsMap(repoPath)
    val files = get_Tracked_Modified_NotAdded(allRepoFiles1, indexMap1, testDir2, repoPath)
    assert(files.isEmpty)


    // Edit file
    FileUtil.writeFile(new File(testDir + separator + "testFile1"), "Goodbye".getBytes.toList, append = true)

    val allRepoFiles2 = getAllRepoFiles(repoPath)
    val indexMap2 = IndexUtil.getIndexAsMap(repoPath)
    val filesAfterEdit = get_Tracked_Modified_NotAdded(allRepoFiles2, indexMap2, testDir2, repoPath)

    assert(filesAfterEdit.length == 1)
    assert(filesAfterEdit.head == file1RelativePath)


    // Add file
    Index.add(Seq(testDir + separator + "testFile1"), repoPath)

    val allRepoFiles3 = getAllRepoFiles(repoPath)
    val indexMap3 = IndexUtil.getIndexAsMap(repoPath)
    val filesAfterAdd = get_Tracked_Modified_NotAdded(allRepoFiles3, indexMap3, testDir2, repoPath)

    assert(filesAfterAdd.isEmpty)
  }


  it should "get tracked and never committed files (relative paths)" in {

    val repoPath = System.getProperty("user.dir") + separator + "repoDir"
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"
    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)
    val commitTree = CommitUtil.getCommitAsMap(repoPath, lastCommitTree)
    val indexMap = IndexUtil.getIndexAsMap(repoPath)

    val files = get_Tracked_NeverCommitted(commitTree, indexMap, testDir2, repoPath)

    assert(files.length == 1)
    assert(files.head == file1RelativePath)

    Commit.commit(repoPath, "commit testFile1")


    val lastCommitTree2 = CommitUtil.getLastCommitTree(repoPath)
    val commitTree2 = CommitUtil.getCommitAsMap(repoPath, lastCommitTree2)
    val indexMap2 = IndexUtil.getIndexAsMap(repoPath)

    val filesAfterCommit = get_Tracked_NeverCommitted(commitTree2, indexMap2, testDir2, repoPath)

    assert(filesAfterCommit.isEmpty)
  }


  it should "get tracked modified and committed files (relative paths)" in {

    val repoDir = System.getProperty("user.dir") + separator + "repoDir"
    val repoPath = RepoUtil.getRepoPath(repoDir).get
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    val lastCommitTree1 = CommitUtil.getLastCommitTree(repoPath)
    val commitTree1 = CommitUtil.getCommitAsMap(repoPath, lastCommitTree1)
    val indexMap1 = IndexUtil.getIndexAsMap(repoPath)

    // No Commit
    val files = get_Tracked_Committed_Modified(commitTree1, indexMap1, testDir2, repoPath)
    assert(files.isEmpty)

    // With Commit
    Commit.commit(repoPath, "first commit")

    /* edit file and add it */
    val testFile1Path = repoPath + separator + "testDir" + separator + "testFile1"
    val testFile2Path = repoPath + separator + "testDir" + separator + "testFile2"

    FileUtil.writeFile(new File(testFile1Path), "Goodbye".getBytes.toList, append = true)
    Index.add(Seq(testFile1Path, testFile2Path), repoPath)

    val indexMap2 = IndexUtil.getIndexAsMap(repoPath)
    val lastCommitTree2 = CommitUtil.getLastCommitTree(repoPath)
    val commitTree2 = CommitUtil.getCommitAsMap(repoPath, lastCommitTree2)

    val filesAfterCommitEditAdd = get_Tracked_Committed_Modified(commitTree2, indexMap2, testDir2, repoPath)
    assert(filesAfterCommitEditAdd.length == 1)
    assert(filesAfterCommitEditAdd.head == file1RelativePath)
  }

  it should "get all the tracked files but not present in working tree (deleted)" in {

    val repoDir = System.getProperty("user.dir") + separator + "repoDir"
    val repoPath = RepoUtil.getRepoPath(repoDir).get
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    new File(file1AbsolutePath).delete()

    val allRepoFiles = getAllRepoFiles(repoPath)
    val indexMap = IndexUtil.getIndexAsMap(repoPath)

    val files = get_Deleted_NotAdded(allRepoFiles, indexMap, testDir2, repoPath)
    assert(files.length == 1)
    assert(files.head == file1RelativePath)
  }

  it should "get all the files removed from index but not committed after been deleted" in {

    val repoPath = System.getProperty("user.dir") + separator + "repoDir"
    val testDir2 = repoPath + separator + "testDir" + separator + "testDir2"

    val file1AbsolutePath = repoPath + separator + "testDir" + separator + "testFile1"
    val file1RelativePath = Paths.get(testDir2).relativize(Paths.get(file1AbsolutePath)).toString

    val indexMap = IndexUtil.getIndexAsMap(repoPath)
    val lastCommitTree = CommitUtil.getLastCommitTree(repoPath)
    val commitTree = CommitUtil.getCommitAsMap(repoPath, lastCommitTree)

    val files = get_Deleted_NotCommitted(commitTree, indexMap, testDir2, repoPath)
    assert(files.isEmpty)

    Commit.commit(repoPath, "commit1")
    FileUtil.createNewFile(repoPath + separator + ".sgit" + separator + "INDEX", "")

    val indexMap2 = IndexUtil.getIndexAsMap(repoPath)
    val lastCommitTree2 = CommitUtil.getLastCommitTree(repoPath)
    val commitTree2 = CommitUtil.getCommitAsMap(repoPath, lastCommitTree2)

    val filesAfterCommit = get_Deleted_NotCommitted(commitTree2, indexMap2, testDir2, repoPath)
    assert(filesAfterCommit.length == 1)
    assert(filesAfterCommit.head == file1RelativePath)

    /*Index.add(Seq(repoPath + separator + "testFile1"), repoPath)
    Commit.commit(repoPath, "commit2")

    val indexMap3 = IndexUtil.getIndexAsMap(repoPath)
    val lastCommitTree3 = CommitUtil.getLastCommitTree(repoPath)
    val commitTree3 = CommitUtil.getCommitAsMap(repoPath, lastCommitTree3)

    val filesAfterAddDeletion = get_Deleted_NotCommitted(commitTree3, indexMap3, testDir2, repoPath)
    assert(filesAfterAddDeletion.isEmpty)*/
  }
}
