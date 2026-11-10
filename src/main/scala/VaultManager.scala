import java.io.{File, FileInputStream, FileOutputStream}
import java.security.SecureRandom
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import javax.crypto.spec.SecretKeySpec

object VaultManager {

  private val ALGORITHM = "AES"
  private val KEY_ALGORITHM = "PBKDF2WithHmacSHA256"
  private val ITERATIONS = 65536
  private val KEY_LENGTH = 256
  private val SALT_LENGTH = 16

  // Генерация ключа шифрования из мастер-пароля
  private def generateKey(masterPassword: String, salt: Array[Byte]): SecretKeySpec = {
    val spec = new PBEKeySpec(masterPassword.toCharArray, salt, ITERATIONS, KEY_LENGTH)
    val factory = SecretKeyFactory.getInstance(KEY_ALGORITHM)
    val keyBytes = factory.generateSecret(spec).getEncoded
    new SecretKeySpec(keyBytes, ALGORITHM)
  }

  // Сохранение хранилища (шифрование и запись в файл)
  def saveVault(filePath: String, records: List[Record], masterPassword: String): Either[Error, Unit] = {
    try {
      val rawData = recordsToJson(records)
      
      val salt = new Array[Byte](SALT_LENGTH)
      new SecureRandom().nextBytes(salt)
      
      val key = generateKey(masterPassword, salt)
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.ENCRYPT_MODE, key)
      val encrypted = cipher.doFinal(rawData.getBytes("UTF-8"))
      
      val fos = new FileOutputStream(new File(filePath))
      fos.write(salt)
      fos.write(encrypted)
      fos.close()

      Right(())
    } catch {
      case e: Exception => Left(Error.SaveFailed(s"Ошибка сохранения: ${e.getMessage}"))
    }
  }
  
  def loadVault(filePath: String, masterPassword: String): Either[Error, List[Record]] = {
    try {
      val file = new File(filePath)
      if (!file.exists()) {
        return Left(Error.StorageNotFound)
      }

      val fis = new FileInputStream(file)
      val salt = new Array[Byte](SALT_LENGTH)
      fis.read(salt)

      val encrypted = LazyList.continually(fis.read).takeWhile(_ != -1).map(_.toByte).toArray
      fis.close()

      val key = generateKey(masterPassword, salt)
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.DECRYPT_MODE, key)
      val decrypted = cipher.doFinal(encrypted)
      val rawData = new String(decrypted, "UTF-8")

      Right(jsonToRecords(rawData))
    } catch {
      case _: javax.crypto.BadPaddingException =>
        Left(Error.WrongPassword)
      case _: javax.crypto.AEADBadTagException =>
        Left(Error.WrongPassword)
      case e: Exception =>
        Left(Error.VaultCorrupted)
    }
  }
  
  private def recordsToJson(records: List[Record]): String = {
    val recordsJson = records.map { r =>
      s"""{"id":"${r.id}","service":"${r.service}","username":"${r.username}","password":"${r.password}","email":"${r.email}","url":"${r.url}","note":"${r.note}","created":"${r.created}","modified":"${r.modified}"}"""
    }.mkString("[", ",", "]")
    recordsJson
  }

  private def jsonToRecords(json: String): List[Record] = {
    if (json.trim == "[]" || json.trim.isEmpty) return List.empty

    val recordRegex = """\{"id":"([^"]+)","service":"([^"]*)","username":"([^"]*)","password":"([^"]*)","email":"([^"]*)","url":"([^"]*)","note":"([^"]*)","created":"([^"]*)","modified":"([^"]*)"\}""".r

    recordRegex.findAllMatchIn(json).map { m =>
      Record(
        id = m.group(1),
        service = m.group(2),
        username = m.group(3),
        password = m.group(4),
        email = m.group(5),
        url = m.group(6),
        note = m.group(7),
        created = m.group(8),
        modified = m.group(9)
      )
    }.toList
  }
}