import java.io.File
import java.time.LocalDateTime

object Main {

  def main(args: Array[String]): Unit = {
    val configResult = ConfigManager.loadConfig("config.json")
    configResult match {
      case Left(error) => println("Критическая ошибка конфигурации."); printError(error)
      case Right(config) =>
        Logger.init(config.logFile)
        Logger.log("INFO", "Конфигурация успешно загружена")
        println("Конфигурация загружена.")
        runApp(config)
    }
  }

  def parseCommand(input: String): Command = {
    val trimmed = input.trim.toLowerCase
    trimmed match {
      case "1" | "create" => Command.Create
      case "2" | "open" => Command.Open
      case "3" | "list" => Command.List
      case "5" | "add" => Command.Add
      case "9" | "lock" => Command.Lock
      case "10" | "reload" => Command.Reload
      case "12" | "exit" => Command.Exit
      case "help" => Command.Help
      case cmd if cmd.startsWith("4 ") || cmd.startsWith("show ") => Command.Show(cmd.substring(cmd.indexOf(" ") + 1).trim)
      case cmd if cmd.startsWith("pass ") => Command.Pass(cmd.substring(cmd.indexOf(" ") + 1).trim)
      case cmd if cmd.startsWith("6 ") || cmd.startsWith("edit ") => Command.Edit(cmd.substring(cmd.indexOf(" ") + 1).trim)
      case cmd if cmd.startsWith("7 ") || cmd.startsWith("remove ") => Command.Remove(cmd.substring(cmd.indexOf(" ") + 1).trim)
      case cmd if cmd.startsWith("8 ") || cmd.startsWith("11 ") || cmd.startsWith("search ") => Command.Search(cmd.substring(cmd.indexOf(" ") + 1).trim)
      case "" => Command.Help
      case _ => Command.Unknown(trimmed)
    }
  }

  private def runApp(config: AppConfig): Unit = {
    var state = AppState()
    var running = true
    println("\n=== Локальное хранилище паролей ===")
    showCommands(state.isUnlocked)

    while (running) {
      if (state.isUnlocked) {
        val idleMinutes = (System.currentTimeMillis() - state.lastActivityTime) / 60000
        if (idleMinutes >= config.automaticLockMinutes) {
          state = state.copy(isUnlocked = false, masterPasswordPlain = None)
          Logger.log("INFO", "Хранилище автоматически заблокировано из-за бездействия")
          println("\n[!] Хранилище автоматически заблокировано из-за бездействия.")
          showCommands(isUnlocked = false)
        }
      }

      print(if (state.isUnlocked) "\nхранилище> " else "заблокировано> ")
      val input = scala.io.StdIn.readLine()

      if (input == null) running = false
      else {
        val command = parseCommand(input)
        if (command != Command.Unknown("")) state = state.copy(lastActivityTime = System.currentTimeMillis())

        command match {
          case Command.Exit => running = false
          case Command.Help => showCommands(state.isUnlocked)

          case Command.Create =>
            val fileExists = new File(config.storageFile).exists()
            if (fileExists) {
              println("Внимание: Файл хранилища уже существует!")
              print("Перезаписать? [да/нет]: ")
              val ans = scala.io.StdIn.readLine().trim.toLowerCase
              if (ans != "да" && ans != "yes") {
                println("Создание отменено.")
              } else {
                new File(config.storageFile).delete()
                print("Введите новый мастер-пароль: "); val p1 = scala.io.StdIn.readLine()
                print("Повторите мастер-пароль: "); val p2 = scala.io.StdIn.readLine()
                AppLogic.createVault(config, p1, p2) match {
                  case Right(s) => state = s; println("Хранилище успешно создано."); showCommands(true)
                  case Left(err) => printError(err)
                }
              }
            } else {
              print("Введите новый мастер-пароль: "); val p1 = scala.io.StdIn.readLine()
              print("Повторите мастер-пароль: "); val p2 = scala.io.StdIn.readLine()
              AppLogic.createVault(config, p1, p2) match {
                case Right(s) => state = s; println("Хранилище успешно создано."); showCommands(true)
                case Left(err) => printError(err)
              }
            }

          case Command.Open =>
            if (state.isUnlocked) println("Хранилище уже открыто. Сначала заблокируйте его командой '9'.")
            else if (state.isBlockedPermanently) printError(Error.MaxAttemptsExceeded)
            else if (!new File(config.storageFile).exists()) println("Ошибка: Файл хранилища не найден.")
            else {
              print("Введите мастер-пароль: "); val password = scala.io.StdIn.readLine()
              AppLogic.openVault(config, password) match {
                case Right(records) =>
                  println("Хранилище успешно открыто.")
                  state = state.copy(isUnlocked = true, failedAttempts = 0, records = records, masterPasswordPlain = Some(password))
                  showCommands(true)
                case Left(Error.WrongPassword) =>
                  val newAttempts = state.failedAttempts + 1
                  println(s"Ошибка: Неверный пароль. Осталось попыток: ${config.maximumUnlockAttempts - newAttempts}")
                  if (newAttempts >= config.maximumUnlockAttempts) { printError(Error.MaxAttemptsExceeded); state = state.copy(isBlockedPermanently = true, failedAttempts = newAttempts) }
                  else state = state.copy(failedAttempts = newAttempts)
                case Left(err) => printError(err)
              }
            }

          case Command.Lock =>
            if (state.isUnlocked) { state = state.copy(isUnlocked = false, masterPasswordPlain = None); println("Хранилище заблокировано."); showCommands(false) }
            else println("Хранилище уже заблокировано.")

          case Command.List =>
            if (checkAccess(state)) {
              val sorted = config.defaultSort match { case "service" => state.records.sortBy(_.service.toLowerCase); case "username" => state.records.sortBy(_.username.toLowerCase); case _ => state.records.sortBy(_.id) }
              println(f"ID   Сервис       Имя пользователя          Изменена")
              sorted.foreach(r => println(f"${r.id}%-4s ${r.service}%-12s ${r.username}%-25s ${r.modified}"))
            }

          case Command.Show(id) => if (checkAccess(state)) state.records.find(_.id == id) match { case Some(r) => println(s"Сервис: ${r.service}\nПароль: ********"); case None => printError(Error.RecordNotFound(id)) }
          case Command.Pass(id) => if (checkAccess(state)) state.records.find(_.id == id) match { case Some(r) => println(s"Пароль: ${r.password}"); case None => printError(Error.RecordNotFound(id)) }

          case Command.Add =>
            if (checkAccess(state)) {
              print("Сервис: "); val s = scala.io.StdIn.readLine().trim
              if (s.nonEmpty && state.isUnlocked) {
                print("Пользователь: "); val u = scala.io.StdIn.readLine().trim; print("Пароль: "); val p = scala.io.StdIn.readLine().trim
                print("Email: "); val e = scala.io.StdIn.readLine().trim; print("URL: "); val url = scala.io.StdIn.readLine().trim; print("Заметка: "); val n = scala.io.StdIn.readLine().trim
                AppLogic.addRecord(config, state, s, u, p, e, url, n) match { case Right(st) => state = st; println("Добавлено."); case Left(err) => printError(err) }
              } else println("Отменено или сессия истекла.")
            }

          case Command.Edit(id) =>
            if (checkAccess(state)) state.records.find(_.id == id) match {
              case Some(r) =>
                print(s"Сервис [${r.service}]: "); val s = scala.io.StdIn.readLine().trim
                if (s.toLowerCase != "отмена" && state.isUnlocked) {
                  print("Пользователь: "); val u = scala.io.StdIn.readLine().trim; print("Пароль: "); val p = scala.io.StdIn.readLine().trim
                  print("Email: "); val e = scala.io.StdIn.readLine().trim; print("URL: "); val url = scala.io.StdIn.readLine().trim; print("Заметка: "); val n = scala.io.StdIn.readLine().trim
                  AppLogic.editRecord(config, state, id, s, u, p, e, url, n) match { case Right(st) => state = st; println("Изменено."); case Left(err) => printError(err) }
                }
              case None => printError(Error.RecordNotFound(id))
            }

          case Command.Remove(id) =>
            if (checkAccess(state)) state.records.find(_.id == id) match {
              case Some(r) =>
                print("Удалить? [да/нет]: ")
                if (scala.io.StdIn.readLine().trim.toLowerCase == "да") {
                  AppLogic.removeRecord(config, state, id) match { case Right(st) => state = st; println("Удалено."); case Left(err) => printError(err) }
                }
              case None => printError(Error.RecordNotFound(id))
            }

          case Command.Search(query) =>
            if (checkAccess(state)) AppLogic.searchRecords(state, query) match {
              case Right(found) => println(s"Найдено: ${found.size}"); found.foreach(r => println(s"${r.id} ${r.service}"))
              case Left(err) => printError(err)
            }

          case Command.Reload =>
            if (state.isUnlocked) println("Сначала заблокируйте хранилище.")
            else ConfigManager.loadConfig("config.json") match { case Right(_) => println("Перезагружено."); case Left(err) => printError(err) }

          case Command.Unknown(text) => println(s"Неизвестная команда: $text")
        }
      }
    }
    Logger.close()
  }

  private def checkAccess(state: AppState): Boolean = if (!state.isUnlocked) { println("Хранилище заблокировано."); false } else true

  private def showCommands(isUnlocked: Boolean): Unit = {
    println("\nДоступные команды:")
    println("1  - Создать новое хранилище")
    println("2  - Открыть существующее хранилище")
    if (isUnlocked) {
      println("3  - Показать список записей")
      println("4 <id> - Показать выбранную запись")
      println("5  - Добавить запись")
      println("6 <id> - Изменить запись")
      println("7 <id> - Удалить запись")
      println("8 <текст> - Найти запись по подстроке")
      println("pass <id> - Показать пароль записи")
      println("9  - Заблокировать хранилище")
    }
    println("10 - Перезагрузить конфигурацию")
    println("12 - Завершить работу")
  }

  // Переводим ADT ошибок в текст для пользователя
  private def printError(err: Error): Unit = err match {
    case Error.WrongPassword => println("Ошибка: Неверный мастер-пароль.")
    case Error.StorageNotFound => println("Ошибка: Файл хранилища не найден.")
    case Error.StorageAlreadyExists => println("Ошибка: Файл уже существует.")
    case Error.RecordNotFound(id) => println(s"Ошибка: Запись с ID '$id' не найдена.")
    case Error.VaultCorrupted => println("Ошибка: Файл поврежден.")
    case Error.MaxAttemptsExceeded => println("Превышено количество попыток. Перезапустите приложение.")
    case Error.EmptySearchQuery => println("Ошибка: Поисковый запрос не задан.")
    case Error.PasswordMismatch => println("Ошибка: Пароли не совпадают.")
    case Error.WeakPassword => println("Ошибка: Пароль слишком короткий.")
    case Error.ConfigError(p, r) => println(s"Ошибка конфига ($p): $r")
    case Error.SaveFailed(r) => println(s"Ошибка сохранения: $r")
    case Error.ValidationError(m) => println(s"Ошибка: $m")
  }
}