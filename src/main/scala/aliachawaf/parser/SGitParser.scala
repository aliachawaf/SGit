package aliachawaf.parser

import aliachawaf.util.ResultUtil._
import aliachawaf._
import aliachawaf.command.{Branch, Commit, Diff, Index, Init, Log, Status, Tag}
import aliachawaf.util.RepoUtil
import scopt.OParser

object SGitParser {

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("sgit"),
      head("sgit", "1.0"),
      help("help")
        .text("These are common SGit commands used in various situations:\n"),

      cmd("init")
        .action((_, c) => c.copy(mode = "init"))
        .text("Create an empty SGit repository or reinitialize an existing one\n"),

      cmd("status")
        .action((_, c) => c.copy(mode = "status"))
        .text("Show the working tree status\n"),

      cmd("diff")
        .action((_, c) => c.copy(mode = "diff"))
        .text("Show changes between working tree and tracked files\n"),

      cmd("add")
        .action((_, c) => c.copy(mode = "add"))
        .text("Add file contents to the index")
        .children(
          arg[String]("<file>...")
            .unbounded()
            .required()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("file to add to stage\n")
        ),
      cmd("commit")
        .action((_, c) => c.copy(mode = "commit"))
        .text("Record changes to the repository")
        .children(
          opt[String]('m', "message")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(arguments = x))
            .text("message describing the commit\n")
        ),

      cmd(name = "log")
        .action((_, c) => c.copy(mode = "log"))
        .text("Show commit logs starting with the newest one")
        .children(
          opt[Unit]('p', "patch")
            .action((_, c) => c.copy(option = "patch"))
            .text("show changes overtime"),
          opt[Unit]('s', "stat")
            .action((_, c) => c.copy(option = "stat"))
            .text("show stats about changes overtime\n")
        ),

      cmd("branch")
        .action((_, c) => c.copy(mode = "branch"))
        .text("Create a new branch")
        .children(
          arg[String]("<name>")
            .optional()
            .maxOccurs(1)
            .action((x, c) => c.copy(name = x))
            .text("name of the new branch\n"),
          opt[Unit]('a', name = "all")
            .action((_, c) => c.copy(verbose = true))
            .text("List all branches and tags\n"),
          opt[Unit]('v', name = "verbose")
            .action((_, c) => c.copy(verbose = true))
            .text("List all branches and tags\n"),
        ),

      cmd("tag")
        .action((_, c) => c.copy(mode = "tag"))
        .text("Create a tag object referencing on the last commit\n")
        .children(
          arg[String]("<tag name>")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(name = x))
            .text("unique name of the tag")
        )
    )
  }

  def parse(currentDirectory: String, repoPath: Option[String], args: Array[String]): String = {
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        config.mode match {

          case "init" => Init.initialize(currentDirectory)

          case "status" =>
            if (RepoUtil.isInRepository(currentDirectory)) Status.status(currentDirectory)
            else notSGitRepository()

          case "diff" =>
            if (RepoUtil.isInRepository(currentDirectory)) Diff.diff(repoPath.get)
            else notSGitRepository()

          case "add" =>
            if (RepoUtil.isInRepository(currentDirectory)) Index.add(config.files, repoPath.get)
            else notSGitRepository()

          case "commit" =>
            if (RepoUtil.isInRepository(currentDirectory)) Commit.commit(repoPath.get, config.arguments)
            else notSGitRepository()

          case "log" =>
            if (RepoUtil.isInRepository(currentDirectory)) Log.log(repoPath.get, config.option)
            else notSGitRepository()

          case "branch" =>
            if (RepoUtil.isInRepository(currentDirectory)) Branch.branch(repoPath.get, config.verbose, config.name)
            else notSGitRepository()

          case "tag" =>
            if (RepoUtil.isInRepository(currentDirectory)) Tag.tag(repoPath.get, config.name)
            else notSGitRepository()

          case _ => "TO DO"
        }
      case _ => ""
      // arguments are bad, error message will have been displayed
    }
  }
}