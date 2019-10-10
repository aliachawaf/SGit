package aliachawaf.util

import java.io.{File, FileOutputStream}
import java.math.BigInteger
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.security.MessageDigest

import scala.annotation.tailrec
import scala.util.matching.Regex

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
    FileUtil.writeFile(f, content.getBytes.toList, true)
  }

  def recursiveListFiles(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_, r))
  }

  def getFilePaths(files: Seq[File], directory: String): List[String] = {
    @tailrec
    def loop(listAcc: List[String], files: Seq[File]): List[String] = {
      files match {
        case Nil => listAcc
        case head :: tail => {
          val list = FileUtil.recursiveListFiles(new File(directory), head.getName.r).filter(f => f.isFile).map(f => f.getAbsolutePath)
          loop(listAcc ++ list, tail)
        }
      }
    }
    loop(List[String](), files)
  }

  def getFileContent(filePath: String): List[String] = {
    val source = scala.io.Source.fromFile(filePath)
    return try source.getLines.toList finally source.close()
  }
}
