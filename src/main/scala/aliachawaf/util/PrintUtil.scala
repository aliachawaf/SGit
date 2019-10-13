package aliachawaf.util

import java.io.File

object PrintUtil {
  def printNotSGitRepository(): String = println("fatal: not a sgit repository (or any of the parent directories): .sgit")

  def printAlreadyInitialized(): Unit = println("Already initialized SGit repository")
  def printInitialized(currentDir: String): Unit = println("Initialized empty Sgit repository in " + currentDir + File.separator + ".sgit" + File.separator
  )

  def printNoIndex(): Unit = println("nothing to commit")

  def printInitResult(result: Boolean, currentDir: String): Unit = {
    result match {
      case true => printInitialized(currentDir)
      case false => printAlreadyInitialized()
    }
  }

  def printNoMatchingFiles(arguments: Seq[String]) : Unit = println("fatal: pathspec ")

}
