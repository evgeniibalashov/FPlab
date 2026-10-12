import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.time.LocalDateTime

object Logger {
  private var writer: PrintWriter = null

  def init(logFile: String): Unit = {
    try {
      // true означает режим дозаписи (append), чтобы журнал не перезаписывался при каждом запуске
      val fos = new FileOutputStream(new File(logFile), true)
      writer = new PrintWriter(new OutputStreamWriter(fos, "UTF-8"))
      log("INFO", "Приложение запущено")
    } catch {
      case e: Exception =>
        println(s"Критическая ошибка: не удалось создать файл журнала: ${e.getMessage}")
    }
  }

  def log(level: String, message: String): Unit = {
    if (writer != null) {
      val timestamp = LocalDateTime.now().format(dateFormat)
      writer.println(s"$timestamp $level  $message")
      writer.flush() // Сразу сбрасываем буфер на диск
    }
  }

  def close(): Unit = {
    if (writer != null) {
      log("INFO", "Приложение завершено")
      writer.close()
    }
  }
}