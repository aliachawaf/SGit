package aliachawaf.objects

import java.security.MessageDigest
import java.math.BigInteger

abstract class SGitObject {
  val idHashed: String
}

object SGitObject {

  def hash(s: String): String = {
    val md = MessageDigest.getInstance("SHA-1")
    val digest = md.digest(s.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }
}

