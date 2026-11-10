import java.io.{File, PrintWriter}
import scala.io.Source

object ConfigManager {
  
  def loadConfig(filePath: String): Either[Error, AppConfig] = {
    try {
      val file = new File(filePath)
      if (!file.exists()) {
        val defaultConfig = AppConfig(
          storageFile = "password-vault.dat",
          logFile = "password-vault.log",
          automaticLockMinutes = 5,
          maximumUnlockAttempts = 3,
          defaultSort = "service",
          confirmBeforeDelete = true
        )
        saveConfig(filePath, defaultConfig)
        return Right(defaultConfig)
      }

      val content = Source.fromFile(file, "UTF-8").getLines().mkString
      val config = AppConfig(
        storageFile = extractString(content, "storageFile"),
        logFile = extractString(content, "logFile"),
        automaticLockMinutes = extractInt(content, "automaticLockMinutes"),
        maximumUnlockAttempts = extractInt(content, "maximumUnlockAttempts"),
        defaultSort = extractString(content, "defaultSort"),
        confirmBeforeDelete = extractBoolean(content, "confirmBeforeDelete")
      )

      validateConfig(config)
    } catch {
      case e: Exception => Left(Error.ConfigError("config.json", s"Ошибка чтения файла: ${e.getMessage}"))
    }
  }

  private def extractString(json: String, key: String): String = {
    val regex = s""""$key"\\s*:\\s*"([^"]+)"""".r
    regex.findFirstMatchIn(json).map(_.group(1)).getOrElse("")
  }

  private def extractInt(json: String, key: String): Int = {
    val regex = s""""$key"\\s*:\\s*([0-9]+)""".r
    regex.findFirstMatchIn(json).map(_.group(1).toInt).getOrElse(0)
  }

  private def extractBoolean(json: String, key: String): Boolean = {
    val regex = s""""$key"\\s*:\\s*(true|false)""".r
    regex.findFirstMatchIn(json).map(_.group(1).toBoolean).getOrElse(false)
  }

  private def validateConfig(config: AppConfig): Either[Error, AppConfig] = {
    if (config.storageFile.isEmpty)
      return Left(Error.ConfigError("storageFile", "Путь не указан. Ожидается: строка."))
    if (config.logFile.isEmpty)
      return Left(Error.ConfigError("logFile", "Путь не указан. Ожидается: строка."))
    if (config.storageFile == config.logFile)
      return Left(Error.ConfigError("storageFile/logFile", "Файлы хранилища и журнала не могут совпадать."))
    if (config.automaticLockMinutes < 1)
      return Left(Error.ConfigError("automaticLockMinutes", s"Получено: ${config.automaticLockMinutes}. Ожидается: целое число не меньше 1."))
    if (config.maximumUnlockAttempts < 1)
      return Left(Error.ConfigError("maximumUnlockAttempts", s"Получено: ${config.maximumUnlockAttempts}. Ожидается: целое число не меньше 1."))
    if (!List("service", "id", "username").contains(config.defaultSort))
      return Left(Error.ConfigError("defaultSort", s"Получено: ${config.defaultSort}. Ожидается: service, id или username."))

    Right(config)
  }

  def saveConfig(filePath: String, config: AppConfig): Unit = {
    val writer = new PrintWriter(new File(filePath), "UTF-8")
    writer.println("{")
    writer.println(s"""  "storageFile": "${config.storageFile}",""")
    writer.println(s"""  "logFile": "${config.logFile}",""")
    writer.println(s"""  "automaticLockMinutes": ${config.automaticLockMinutes},""")
    writer.println(s"""  "maximumUnlockAttempts": ${config.maximumUnlockAttempts},""")
    writer.println(s"""  "defaultSort": "${config.defaultSort}",""")
    writer.println(s"""  "confirmBeforeDelete": ${config.confirmBeforeDelete}""")
    writer.println("}")
    writer.close()
  }
}