package aliachawaf

import java.io.File
import scopt.OParser

case class Config(
   foo: Int = -1,
   out: File = new File("."),
   xyz: Boolean = false,
   libName: String = "",
   maxCount: Int = -1,
   verbose: Boolean = false,
   debug: Boolean = false,
   mode: String = "",
   files: Seq[File] = Seq(),
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
          arg[File]("<file>...")
            .unbounded()
            .optional()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("file to add to stage")
        ),
    )
  }

  val currentDirectory = System.getProperty("user.dir")

  // OParser.parse returns Option[app.Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config.mode match {
        case "init" => {
          Repository.initialize(currentDirectory)
        }
        case "add" => {
          if (Repository.isInitialized(currentDirectory) && Repository.getPathSGit(currentDirectory).isDefined) {
            Index.add(config.files, currentDirectory)
          } else {
            // TO DO print NOT SGIT REPO
          }
        }
        case _ =>
          printNotFound(config.mode)
      }
    case _ =>
    // arguments are bad, error message will have been displayed
  }


  // Utils _ Printing methods
  def printNotFound(command: String): Unit = println(command + ": command not found.")
  def printNotSGitCommand(command: String): Unit = println("sgit: '" + command + "' is not a sgit command. See 'sgit --help'.")
}