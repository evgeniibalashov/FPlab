import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.time.LocalDateTime

object Logger {
  // Заменяем null на None
  private var writer: Option[PrintWriter] = None

  def init(logFile: String): Unit = {
    try {
      val fos = new FileOutputStream(new File(logFile), true)
      writer = Some(new PrintWriter(new OutputStreamWriter(fos, "UTF-8")))
      log("INFO", "Приложение запущено")
    } catch {
      case e: Exception =>
        println(s"Критическая ошибка: не удалось создать файл журнала: ${e.getMessage}")
    }
  }

  def log(level: String, message: String): Unit = {
    writer.foreach { w =>
      val timestamp = LocalDateTime.now().format(dateFormat)
      w.println(s"$timestamp $level  $message")
      w.flush()
    }
  }

  def close(): Unit = {
    writer.foreach { w =>
      log("INFO", "Приложение завершено")
      w.close()
    }
    
    writer = None
  }
}