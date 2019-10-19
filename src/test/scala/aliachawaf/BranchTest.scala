package aliachawaf

import java.io.File

import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class BranchTest extends FlatSpec with BeforeAndAfterEach {
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

  // We delete the sgit repository and files created after each test
  override def afterEach(): Unit = {
    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get + File.separator + ".sgit"
    Directory(new File(repoPath)).deleteRecursively()
    Directory(new File(repoPath + File.separator + "testDir")).deleteRecursively()
  }

  "The branch command" should "create a file branch in .sgit/branches with the right name and content" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get

    Commit.commit(repoPath, "message")
    val lastCommit = CommitUtil.getLastCommit(repoPath, BranchUtil.getCurrentBranch(repoPath)).get

    val branchResult = Branch.createNewBranch(repoPath, "newBranch")
    val branchPath = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + "newBranch"

    assert(branchResult == "Branch 'newBranch' created")
    assert(new File(branchPath).exists())
    assert((FileUtil.getFileContent(branchPath) mkString "\n") == lastCommit)
  }

  it should "not create a branch if there is no commit" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get

    val branchResult = Branch.createNewBranch(repoPath, "newBranch")
    val branchPath = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + "newBranch"

    assert(!new File(branchPath).exists())
    assert(branchResult == "fatal: Not a valid object name: 'master'.")
  }

  it should "not update a branch if it already exists" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get

    Commit.commit(repoPath, "message")

    Branch.createNewBranch(repoPath, "newBranch")
    val branchPath = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + "newBranch"
    val branchContent = FileUtil.getFileContent(branchPath)

    Index.add(Seq("testDir/testFile2"), repoPath)
    Commit.commit(repoPath, "message")
    CommitUtil.getLastCommit(repoPath, BranchUtil.getCurrentBranch(repoPath)).get

    val branchResult = Branch.createNewBranch(repoPath, "newBranch")
    val branchContent2 = FileUtil.getFileContent(branchPath)

    assert(branchContent == branchContent2)
    assert(branchResult == "fatal: branch named 'newBranch' already exists")
  }

  it should "get all the branches and tags" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = Repository.getRepoPath(currentDir).get

    Commit.commit(repoPath, "commit1")
    Branch.createNewBranch(repoPath, "branch1")

    Index.add(Seq("testDir/testFile2"), repoPath)
    Commit.commit(repoPath, "commit2")
    Branch.createNewBranch(repoPath, "branch2")

    Tag.tag(repoPath, "tag1")
    println(Branch.branchAV(repoPath))
  }




}
