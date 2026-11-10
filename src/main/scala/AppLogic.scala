import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

object AppLogic {

  private val dateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  def createVault(config: AppConfig, p1: String, p2: String): Either[Error, AppState] = {
    if (p1 != p2) Left(Error.PasswordMismatch)
    else if (p1.length < 4) Left(Error.WeakPassword)
    else if (new File(config.storageFile).exists()) Left(Error.StorageAlreadyExists)
    else {

      // Используем map
      VaultManager.saveVault(config.storageFile, List.empty, p1).map { _ =>
        AppState(isUnlocked = true, failedAttempts = 0, masterPasswordPlain = Some(p1))
      }
    }
  }

  def openVault(config: AppConfig, password: String): Either[Error, List[Record]] = {
    if (!new File(config.storageFile).exists()) Left(Error.StorageNotFound)
    else VaultManager.loadVault(config.storageFile, password)
  }

  def addRecord(config: AppConfig, state: AppState, service: String, username: String,
                password: String, email: String, url: String, note: String): Either[Error, AppState] = {
    if (service.isEmpty) Left(Error.ValidationError("Сервис не может быть пустым"))
    else {
      val now = LocalDateTime.now().format(dateFormat)
      val newRecord = Record(
        UUID.randomUUID().toString.substring(0, 8),
        service, username, password, email, url, note, now, now
      )
      val updatedRecords = state.records :+ newRecord

      VaultManager.saveVault(config.storageFile, updatedRecords, state.masterPasswordPlain.getOrElse("")).map { _ =>
        state.copy(records = updatedRecords)
      }
    }
  }

  def editRecord(config: AppConfig, state: AppState, id: String, service: String,
                 username: String, password: String, email: String, url: String, note: String): Either[Error, AppState] = {

    // Испоьзуем flatMap
    state.records.find(_.id == id).toRight(Error.RecordNotFound(id)).flatMap { r =>

      val updatedRecord = r.copy(
        service = if (service.nonEmpty) service else r.service,
        username = if (username.nonEmpty) username else r.username,
        password = password, // Сюда Main передаст либо новый пароль, либо старый r.password
        email = if (email.nonEmpty) email else r.email,
        url = if (url.nonEmpty) url else r.url,
        note = if (note.nonEmpty) note else r.note
      ).updateModified()

      val updatedRecords = state.records.map(rec => if (rec.id == id) updatedRecord else rec)

      VaultManager.saveVault(config.storageFile, updatedRecords, state.masterPasswordPlain.getOrElse("")).map { _ =>
        state.copy(records = updatedRecords)
      }
    }
  }

  def removeRecord(config: AppConfig, state: AppState, id: String): Either[Error, AppState] = {
    val updatedRecords = state.records.filterNot(_.id == id)

    if (updatedRecords.size == state.records.size) Left(Error.RecordNotFound(id))
    else {
      VaultManager.saveVault(config.storageFile, updatedRecords, state.masterPasswordPlain.getOrElse("")).map { _ =>
        state.copy(records = updatedRecords)
      }
    }
  }

  def searchRecords(state: AppState, query: String): Either[Error, List[Record]] = {
    if (query.isEmpty) Left(Error.EmptySearchQuery)
    else {
      val lowerQuery = query.toLowerCase
      val found = state.records.filter(r =>
        r.service.toLowerCase.contains(lowerQuery) ||
          r.username.toLowerCase.contains(lowerQuery) ||
          r.url.toLowerCase.contains(lowerQuery)
      )
      Right(found)
    }
  }
}