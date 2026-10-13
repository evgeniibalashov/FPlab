import java.io.{File, FileInputStream, FileOutputStream}
import java.security.MessageDigest
import java.util.Base64
import javax.crypto.{Cipher, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import scala.io.Source

object VaultManager {
  private val ALGORITHM = "AES"
  private val ITERATIONS = 65536
  private val KEY_LENGTH = 256
  private val SALT = "FixedSaltForLab2026".getBytes // Для учебной лабы фиксированная соль допустима

  // Преобразуем мастер-пароль в ключ шифрования
  private def generateKey(masterPassword: String): SecretKey = {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    val spec = new PBEKeySpec(masterPassword.toCharArray, SALT, ITERATIONS, KEY_LENGTH)
    new SecretKeySpec(factory.generateSecret(spec).getEncoded, ALGORITHM)
  }

  // Хэшируем мастер-пароль для проверки (чтобы не хранить его в открытом виде)
  def hashPassword(password: String): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(password.getBytes("UTF-8"))
    hash.map("%02x".format(_)).mkString
  }

  def saveVault(filePath: String, records: List[Record], masterPassword: String): Either[String, Unit] = {
    try {
      val key = generateKey(masterPassword)
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.ENCRYPT_MODE, key)

      // Преобразуем записи в простой текстовый формат (CSV-подобный для простоты парсинга)
      val rawData = records.map(r =>
        s"${r.id}|${r.service}|${r.username}|${r.password}|${r.email}|${r.url}|${r.note}|${r.created}|${r.modified}"
      ).mkString("\n")

      val encrypted = cipher.doFinal(rawData.getBytes("UTF-8"))
      val fos = new FileOutputStream(filePath)
      fos.write(encrypted)
      fos.close()
      Right(())
    } catch {
      case e: Exception => Left(s"Ошибка сохранения хранилища: ${e.getMessage}")
    }
  }

  def loadVault(filePath: String, masterPassword: String): Either[String, List[Record]] = {
    try {
      val file = new File(filePath)
      if (!file.exists()) return Right(List.empty)

      val key = generateKey(masterPassword)
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.DECRYPT_MODE, key)

      val fis = new FileInputStream(filePath)
      val encrypted = LazyList.continually(fis.read).takeWhile(_ != -1).map(_.toByte).toArray
      fis.close()

      val decrypted = new String(cipher.doFinal(encrypted), "UTF-8")

      // Парсим строки обратно в Record
      val records = decrypted.split("\n").filter(_.nonEmpty).map { line =>
        val parts = line.split("\\|", -1) // -1 чтобы сохранить пустые строки в конце
        Record(parts(0), parts(1), parts(2), parts(3), parts(4), parts(5), parts(6), parts(7), parts(8))
      }.toList

      Right(records)
    } catch {
      case _: javax.crypto.BadPaddingException | _: javax.crypto.IllegalBlockSizeException =>
        Left("Ошибка: Неверный мастер-пароль или файл хранилища поврежден.")
      case e: Exception =>
        Left(s"Ошибка чтения хранилища: ${e.getMessage}")
    }
  }
}