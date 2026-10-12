import scala.io.Source
import java.io.{File, PrintWriter}

object ConfigManager {

  // Простой парсер для нашего конкретного формата JSON
  def loadConfig(filePath: String): Either[String, AppConfig] = {
    try {
      val file = new File(filePath)
      if (!file.exists()) {
        // Если файла нет, создаем конфигурацию по умолчанию
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
      case e: Exception => Left(s"Ошибка чтения конфигурации: ${e.getMessage}")
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

  private def validateConfig(config: AppConfig): Either[String, AppConfig] = {
    if (config.storageFile.isEmpty) return Left("Параметр: storageFile. Получено: пусто. Ожидается: путь к файлу.")
    if (config.logFile.isEmpty) return Left("Параметр: logFile. Получено: пусто. Ожидается: путь к файлу.")
    if (config.storageFile == config.logFile) return Left("Параметр: storageFile/logFile. Ошибка: файлы хранилища и журнала не могут совпадать.")
    if (config.automaticLockMinutes < 1) return Left(s"Параметр: automaticLockMinutes. Получено: ${config.automaticLockMinutes}. Ожидается: целое число не меньше 1.")
    if (config.maximumUnlockAttempts < 1) return Left(s"Параметр: maximumUnlockAttempts. Получено: ${config.maximumUnlockAttempts}. Ожидается: целое число не меньше 1.")
    if (!List("service", "id", "username").contains(config.defaultSort)) return Left(s"Параметр: defaultSort. Получено: ${config.defaultSort}. Ожидается: service, id или username.")

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