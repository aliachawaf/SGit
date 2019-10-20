package aliachawaf

import java.io.File

import aliachawaf.command.Commit.commit
import aliachawaf.command.{Index, Init}
import aliachawaf.util.BranchUtil._
import aliachawaf.util.CommitUtil._
import aliachawaf.util.{BranchUtil, FileUtil, ObjectUtil, RepoUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class CommitTest extends FlatSpec with BeforeAndAfterEach {

  /** Before each test, we initialize the sgit repository with test files */
  override def beforeEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    Init.initialize(currentDir)

    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val testDir = repoPath + File.separator + "testDir"
    new File(testDir).mkdir()
    FileUtil.createNewFile(testDir + File.separator + "testFile1", "Hello, world!")
    FileUtil.createNewFile(testDir + File.separator + "testFile2", "Bonjour tout le monde!")

    Index.add(Seq("testDir/testFile1"), repoPath)
  }

  // We delete the sgit repository and files created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get
    Directory(new File(repoPath + File.separator + ".sgit")).deleteRecursively()
    Directory(new File(repoPath + File.separator + "testDir")).deleteRecursively()
  }

  "A Commit" should "create the branch in .sgit/branches if it is its first commit" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val branchPath = repoPath + File.separator + ".sgit" + File.separator + BranchUtil.getCurrentBranch(repoPath)

    assert(!new File(branchPath).exists())
    commit(repoPath, "message")
    assert(new File(branchPath).exists())
  }

  it should "not create commit object if the previous commit is the same" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val branch = BranchUtil.getCurrentBranch(repoPath)

    assert(getLastCommit(repoPath, branch).isEmpty)
    commit(repoPath, "message")

    val lastCommit = getLastCommit(repoPath, branch)
    assert(lastCommit.isDefined)

    commit(repoPath, "message")
    val sameCommit = getLastCommit(repoPath, branch)

  }

  it should "create commit in .sgit/objects with the right content" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val branch = BranchUtil.getCurrentBranch(repoPath)

    commit(repoPath, "first commit")
    val firstCommit = getLastCommit(repoPath, branch)
    val firstCommitContent = ObjectUtil.getObjectContent(repoPath, firstCommit.get)

    assert(firstCommitContent.length == 2)
    assert(firstCommitContent.head.startsWith("tree "))
    assert(firstCommitContent(1) == "message first commit")

    Index.add(Seq("testDir/testFile2"), repoPath)
    commit(repoPath, "second commit")
    val secondCommit = getLastCommit(repoPath, branch)
    val secondCommitContent = ObjectUtil.getObjectContent(repoPath, secondCommit.get)
    assert(secondCommitContent.length == 3)
    assert(secondCommitContent(1) == ("parent " + firstCommit.get))
    assert(secondCommitContent(2) == "message second commit")
  }

  it should "create all trees of the commit tree in .sgit/objects" in {
  }

  it should "update the current branch with the commit" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get
    val branch = getCurrentBranch(repoPath)

    commit(repoPath, "commit1")
    val commit1 = getLastCommit(repoPath, branch).get

    Index.add(Seq("testDir/testFile2"), repoPath)
    commit(repoPath, "commit2")
    val commit2 = getLastCommit(repoPath, branch).get

    assert(commit1 != commit2)
  }
}
