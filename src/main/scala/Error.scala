
sealed trait Error

object Error {
  
  case object WrongPassword extends Error

  case object StorageNotFound extends Error

  case object StorageAlreadyExists extends Error

  case object VaultCorrupted extends Error

  case object MaxAttemptsExceeded extends Error

  case object EmptySearchQuery extends Error

  case object PasswordMismatch extends Error

  case object WeakPassword extends Error
  
  case class RecordNotFound(id: String) extends Error

  case class ConfigError(parameter: String, reason: String) extends Error

  case class SaveFailed(reason: String) extends Error

  case class ValidationError(message: String) extends Error
}