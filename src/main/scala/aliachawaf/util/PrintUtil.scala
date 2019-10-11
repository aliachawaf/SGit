package aliachawaf.util

object PrintUtil {
  def printNotSGitRepository(): Unit = println("fatal: not a sgit repository (or any of the parent directories): .sgit")

  def printAlreadyInitialized(): Unit = println("Already initialized SGit repository")

  def printNoIndex(): Unit = println("nothing to commit")

}
