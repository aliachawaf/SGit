package aliachawaf.parser

import java.io.File

import aliachawaf.util.ResultUtil._
import aliachawaf.{Branch, Commit, Diff, Index, Log, Repository, Status, Tag}
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
      cmd("status")
        .action((_, c) => c.copy(mode = "status"))
        .text("Show the working tree status\n"),
      cmd("tag")
        .action((_, c) => c.copy(mode = "tag"))
        .text("Create a tag object referencing on the last commit\n")
        .children(
          arg[String]("<tag name>")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(name = x))
            .text("unique name of the tag")
        ),
      cmd("diff")
        .action((_, c) => c.copy(mode = "diff"))
        .text("Show changes between working tree and tracked files\n"),

      cmd("branch")
        .action((_, c) => c.copy(mode = "branch"))
        .text("Create a new branch")
        .children(
          arg[String]("<name>")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(name = x))
            .text("name of the new branch\n")
        ),

      cmd(name = "log")
        .action((_, c) => c.copy(mode = "log"))
        .text("Show commit logs started with newest")
        .children(
          opt[Unit]('p', "patch")
            .action((_, c) => c.copy(option = "patch"))
            .text("Show changes overtime"),
          opt[Unit]('s', "stat")
            .action((_, c) => c.copy(option = "stat"))
            .text("Show stats about changes overtime\n")
        )
    )
  }

  def parse(currentDirectory: String, repoPath: Option[String], args: Array[String]): String = {
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        config.mode match {

          case "init" => Repository.initialize(currentDirectory)

          case "add" =>
            if (Repository.isInRepository(currentDirectory)) Index.add(config.files, repoPath.get)
            else notSGitRepository()

          case "commit" =>
            if (Repository.isInRepository(currentDirectory)) Commit.commit(repoPath.get, config.arguments)
            else notSGitRepository()

          case "status" =>
            if (Repository.isInRepository(currentDirectory)) Status.status(currentDirectory)
            else notSGitRepository()

          case "tag" =>
            if (Repository.isInRepository(currentDirectory)) Tag.tag(repoPath.get, config.name)
            else notSGitRepository()

          case "branch" =>
            if (Repository.isInRepository(currentDirectory)) Branch.createNewBranch(repoPath.get, config.name)
            else notSGitRepository()

          case "diff" =>
            if (Repository.isInRepository(currentDirectory)) Diff.diff(repoPath.get)
            else notSGitRepository()

          case "log" =>
            if (Repository.isInRepository(currentDirectory)) Log.log(repoPath.get, config.option)
            else notSGitRepository()

          case _ => "TO DO"
        }
      case _ => "TO DO"
      // arguments are bad, error message will have been displayed
    }
  }
}