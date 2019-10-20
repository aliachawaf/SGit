package aliachawaf

import java.io.File

import aliachawaf.command.{Commit, Index, Init, Log}
import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, ObjectUtil, RepoUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class LogTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    Init.initialize(currentDir)

    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + File.separator + "testFile1", "Hello, world!")
    FileUtil.createNewFile(testDir + File.separator + "testFile2", "Bonjour tout le monde!")
  }

  // We delete the sgit repository and files created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get + File.separator + ".sgit"
    Directory(new File(repoPath)).deleteRecursively()
    Directory(new File(repoPath + File.separator + "testDir")).deleteRecursively()
  }

  "The Log command" should "get all the commits for the current branch" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val currentBranch = BranchUtil.getCurrentBranch(repoPath)

    Index.add(Seq("testDir/testFile1"), repoPath)
    Commit.commit(repoPath, "first commit")

    val firstCommitHash = CommitUtil.getLastCommit(repoPath, currentBranch).get
    val firstCommitContent = ObjectUtil.getObjectContent(repoPath, firstCommitHash) mkString "\n"

    Index.add(Seq("testDir/testFile2"), repoPath)
    Commit.commit(repoPath, "second commit")

    val secondCommitHash = CommitUtil.getLastCommit(repoPath, currentBranch).get
    val secondCommitContent = ObjectUtil.getObjectContent(repoPath, secondCommitHash) mkString "\n"

    val logResult = Log.getCommitLog(repoPath, secondCommitHash)
    val expectedLogResult = List((firstCommitHash, firstCommitContent), (secondCommitHash, secondCommitContent))
    assert(logResult == expectedLogResult)
  }
}
