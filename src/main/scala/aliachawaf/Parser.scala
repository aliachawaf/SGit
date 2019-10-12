package aliachawaf

import java.io.File
import scopt.OParser
import aliachawaf.util.PrintUtil._

case class Config(
                   foo: Int = -1,
                   out: File = new File("."),
                   xyz: Boolean = false,
                   libName: String = "",
                   maxCount: Int = -1,
                   verbose: Boolean = false,
                   debug: Boolean = false,
                   mode: String = "",
                   arguments: String = "",
                   files: Seq[String] = Seq(),
                   keepalive: Boolean = false,
                   jars: Seq[File] = Seq(),
                   kwargs: Map[String, String] = Map()
                 )

object Parser extends App {

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
        )
    )
  }

  val currentDirectory = System.getProperty("user.dir")
  val repoPath = Repository.getRepoPath(currentDirectory)

  // OParser.parse returns Option[app.Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config.mode match {
        case "init" => printInitResult(Repository.initialize(currentDirectory), currentDirectory)
        case "add" => {
          if (Repository.isInRepository(currentDirectory)) Index.add(config.files, currentDirectory, repoPath.get)
          else printNotSGitRepository()
        }
        case "commit" => {
          if (Repository.isInRepository(currentDirectory)) {
            if (Repository.hasIndexFile(repoPath.get)) println(Commit.commit(repoPath.get, config.arguments))
            else printNoIndex()
          }
          else printNotSGitRepository()
        }
        case _ =>
        //
      }

    case _ =>
    // arguments are bad, error message will have been displayed
  }
}