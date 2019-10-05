package aliachawaf.util

import java.io.{BufferedWriter, File, FileOutputStream}

object FileUtil {

  /* Write in the given file the given content */
  def writeFile(file: File, data: Seq[Byte]): Unit = {
    val f = new FileOutputStream(file)
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
    FileUtil.writeFile(f, content.getBytes.toList)
  }
}
