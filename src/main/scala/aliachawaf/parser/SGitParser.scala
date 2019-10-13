package aliachawaf.parser

import java.io.File
import aliachawaf.util.ResultUtil._
import aliachawaf.{Commit, Index, Repository, Status}
import scopt.OParser

object SGitParser {

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("sgit"),
      head("sgit", "1.x"),
      help("help")
        .text("this is sgit"),
      cmd("init")
        .action((_, c) => c.copy(mode = "init"))
        .text("create a sgit repository"),
      cmd("add")
        .action((_, c) => c.copy(mode = "add"))
        .text("adds the given files to the stage")
        .children(
          arg[String]("<file>...")
            .unbounded()
            .required()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("file to add to stage")
        ),
      cmd("commit")
        .action((_, c) => c.copy(mode = "commit"))
        .text("create a new commit with staged files")
        .children(
          opt[String]('m', "message")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(arguments = x))
        ),
      cmd("status")
        .action((_, c) => c.copy(mode = "status"))
        .text("get status of the repository")
    )
  }

  // OParser.parse returns Option[app.Config]
  // TODO fix prints
  def parse(currentDirectory: String, repoPath: Option[String], args: Array[String]): String = {
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        config.mode match {

          case "init" => Repository.initialize(currentDirectory)

          case "add" =>
            if (Repository.isInRepository(currentDirectory)) Index.add(config.files, repoPath.get)
            else notSGitRepository()

          case "commit" =>
            if (Repository.isInRepository(currentDirectory)) {
              if (Repository.hasIndexFile(repoPath.get)) Commit.commit(repoPath.get, config.arguments)
              else noIndex()
            }
            else notSGitRepository()

          case "status" =>
            if (Repository.isInRepository(currentDirectory)) Status.status(currentDirectory)
            else notSGitRepository()

          case _ => "TO DO"
          //
        }

      case _ => "TO DO"
      // arguments are bad, error message will have been displayed
    }
  }
}