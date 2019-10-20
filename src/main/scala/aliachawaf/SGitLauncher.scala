package aliachawaf
import aliachawaf.parser.SGitParser
import aliachawaf.util.RepoUtil

object SGitLauncher extends App {

  val currentDirectory = System.getProperty("user.dir")
  val repoPath = RepoUtil.getRepoPath(currentDirectory)

  val sgitResult = SGitParser.parse(currentDirectory, repoPath, args)

  println(sgitResult)
}
