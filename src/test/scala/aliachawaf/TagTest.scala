package aliachawaf

import java.io.File

import aliachawaf.command.{Commit, Index, Init, Tag}
import aliachawaf.util.{BranchUtil, CommitUtil, FileUtil, RepoUtil}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

import scala.reflect.io.Directory

class TagTest extends FlatSpec with BeforeAndAfterEach {

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
    val repoPath = RepoUtil.getRepoPath(currentDir).get + File.separator + ".sgit"
    Directory(new File(repoPath)).deleteRecursively()
    Directory(new File(repoPath + File.separator + "testDir")).deleteRecursively()
  }

  "The tag command" should "create a file tag in .sgit/tags with the right name and content" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get

    Commit.commit(repoPath, "message")
    val lastCommit = CommitUtil.getLastCommit(repoPath, BranchUtil.getCurrentBranch(repoPath)).get

    val tagResult = Tag.tag(repoPath, "tagTest")
    val tagPath = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + "tagTest"

    assert(tagResult == "Tag 'tagTest' created")
    assert(new File(tagPath).exists())
    assert((FileUtil.getFileContent(tagPath) mkString "\n") == lastCommit)
  }

  it should "not create a tag if there is no commit" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get

    val tagResult = Tag.tag(repoPath, "tagTest")

    val tagPath = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + "tagTest"
    assert(!new File(tagPath).exists())
    assert(tagResult == "fatal: Failed to resolve 'HEAD' as a valid ref (i.e. there is no commit to tag).")
  }

  it should "not update a tag if it already exists" in {

    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(currentDir).get

    Commit.commit(repoPath, "message")

    Tag.tag(repoPath, "tagTest")
    val tagPath = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + "tagTest"
    val tagContent = FileUtil.getFileContent(tagPath)

    Index.add(Seq("testDir/testFile2"), repoPath)
    Commit.commit(repoPath, "message")
    CommitUtil.getLastCommit(repoPath, BranchUtil.getCurrentBranch(repoPath)).get

    val tagResult = Tag.tag(repoPath, "tagTest")
    val tagContent2 = FileUtil.getFileContent(tagPath)

    assert(tagContent == tagContent2)
    assert(tagResult == "fatal: tag 'tagTest' already exists")
  }
}
