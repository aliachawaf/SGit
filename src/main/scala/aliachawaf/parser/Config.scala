package aliachawaf.parser

import java.io.File

case class Config(
                   name: String = "",
                   option: String = "",
                   verbose: Boolean = false,
                   mode: String = "",
                   arguments: String = "",
                   files: Seq[String] = Seq(),
                 )
