package aliachawaf
import aliachawaf.parser.SGitParser

object SGitLauncher extends App {

  val currentDirectory = System.getProperty("user.dir")
  val repoPath = Repository.getRepoPath(currentDirectory)

  val sgitResult = SGitParser.parse(currentDirectory, repoPath, args)

  println(sgitResult)
}
