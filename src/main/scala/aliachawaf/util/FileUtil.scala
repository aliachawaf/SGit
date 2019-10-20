package aliachawaf.util

import java.io.{File, FileOutputStream}

object FileUtil {

  /**
   * Write in the given file the given content
   *
   * @param file   : file in which we want to write
   * @param data   : content to write
   * @param append : true to append new content, false to overwrite old content
   */
  def writeFile(file: File, data: Seq[Byte], append: Boolean): Unit = {
    val f = new FileOutputStream(file, append)
    try {
      f.write(data.toArray)
    } finally {
      f.close()
    }
  }

  /**
   * Create a file with the given content
   *
   * @param path    : path of the new file
   * @param content : content of the new file
   */
  def createNewFile(path: String, content: String): Unit = {
    val f = new File(path)
    f.createNewFile()
    FileUtil.writeFile(f, content.getBytes.toList, append = false)
  }

  /**
   * Get all th
   *
   * @param f : folder we want to recover all its files
   * @return all the files contained by the given directory
   */
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

  /**
   * Read a file
   *
   * @param filePath : path of the file to read
   * @return the content of the given file as a list of its lines
   */
  def getFileContent(filePath: String): List[String] = {
    if (new File(filePath).exists() && new File(filePath).isFile) {
      val source = scala.io.Source.fromFile(filePath)
      return try source.getLines.toList finally source.close()
    }
    else List()
  }
}
