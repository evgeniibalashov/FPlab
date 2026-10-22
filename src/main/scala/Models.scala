import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

val dateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

case class AppConfig(
                      storageFile: String,
                      logFile: String,
                      automaticLockMinutes: Int,
                      maximumUnlockAttempts: Int,
                      defaultSort: String,
                      confirmBeforeDelete: Boolean
                    )

case class Record(
                   id: String,
                   service: String,
                   username: String,
                   password: String,
                   email: String,
                   url: String,
                   note: String,
                   created: String,
                   modified: String
                 ) {
  def updateModified(): Record = {
    this.copy(modified = LocalDateTime.now().format(dateFormat))
  }
}

case class AppState(
                     isUnlocked: Boolean = false,
                     failedAttempts: Int = 0,
                     isBlockedPermanently: Boolean = false,
                     lastActivityTime: Long = System.currentTimeMillis(),
                     records: List[Record] = List.empty,
                     masterPasswordPlain: String = "" // Хранится только в оперативной памяти, не в файлах
                   )