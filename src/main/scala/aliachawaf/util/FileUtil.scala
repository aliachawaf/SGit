package aliachawaf.util

import java.io.{File, FileOutputStream}

object FileUtil {

  /* Write in the given file the given content */
  def writeFile(file: File, data: Seq[Byte], append: Boolean): Unit = {
    val f = new FileOutputStream(file, append)
    try {
      f.write(data.toArray)
    } finally {
      f.close()
    }
  }

  /* Create a file with the given content. */
  def createNewFile(path: String, content: String): Unit = {
    val f = new File(path)
    f.createNewFile()
    FileUtil.writeFile(f, content.getBytes.toList, false)
  }

  def recursiveListFiles(f: File): Array[File] = {
    f.getName match {
      case "." =>
        val these = f.listFiles
        val theseClean = these.map(file => new File(file.getPath.slice(2, file.getPath.length)))
        theseClean ++ theseClean.filter(_.isDirectory).flatMap(recursiveListFiles _)
      case _ =>
        val these = f.listFiles
        these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles _)
    }
  }

  def getFileContent(filePath: String): List[String] = {
    if (new File(filePath).exists() && new File(filePath).isFile) {
      val source = scala.io.Source.fromFile(filePath)
      return try source.getLines.toList finally source.close()
    }
    else List()
  }
}
