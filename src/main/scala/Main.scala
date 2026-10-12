import java.io.File
import java.time.LocalDateTime
import java.util.UUID

object Main {
  def main(args: Array[String]): Unit = {
    val configResult = ConfigManager.loadConfig("config.json")
    configResult match {
      case Left(error) =>
        println("Ошибка конфигурации.\n" + error)
      case Right(config) =>
        Logger.init(config.logFile)
        Logger.log("INFO", "Конфигурация успешно загружена")
        println("Конфигурация загружена.")
        runApp(config)
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
          state = state.copy(isUnlocked = false, masterPasswordPlain = "")
          Logger.log("INFO", "Хранилище автоматически заблокировано из-за бездействия")
          println("\n[!] Хранилище автоматически заблокировано из-за бездействия.")
          showCommands(isUnlocked = false)
        }
      }

      print(if (state.isUnlocked) "\nхранилище> " else "заблокировано> ")
      val input = scala.io.StdIn.readLine()

      if (input == null) {
        running = false
      } else {
        val command = input.trim.toLowerCase

        val recognized = command match {
          case "exit" | "12" => true
          case "help" => true
          case "create" | "1" => true
          case "open" | "2" => true
          case "lock" | "9" => true
          case "list" | "3" => state.isUnlocked
          case cmd if cmd.startsWith("show ") || cmd.startsWith("4 ") => state.isUnlocked
          case cmd if cmd.startsWith("pass ") => state.isUnlocked
          case "add" | "5" => state.isUnlocked
          case cmd if cmd.startsWith("edit ") || cmd.startsWith("6 ") => state.isUnlocked
          case cmd if cmd.startsWith("remove ") || cmd.startsWith("7 ") => state.isUnlocked
          case cmd if cmd.startsWith("search ") || cmd.startsWith("8 ") || cmd.startsWith("11 ") => state.isUnlocked
          case "reload" | "10" => true
          case _ => false
        }

        if (recognized) {
          state = state.copy(lastActivityTime = System.currentTimeMillis())
        }

        command match {
          case "exit" | "12" =>
            running = false

          case "help" =>
            showCommands(state.isUnlocked)

          case "create" | "1" =>
            val fileExists = new File(config.storageFile).exists()
            if (fileExists) {
              println("Внимание: Файл хранилища уже существует!")
              print("Перезаписать? [да/нет]: ")
              val ans = scala.io.StdIn.readLine().trim.toLowerCase
              if (ans != "да" && ans != "yes") {
                println("Создание отменено.")
              } else {
                handleCreate(config, state) match {
                  case Some(newState) =>
                    state = newState
                    showCommands(isUnlocked = true)
                  case None =>
                }
              }
            } else {
              handleCreate(config, state) match {
                case Some(newState) =>
                  state = newState
                  showCommands(isUnlocked = true)
                case None =>
              }
            }

          case "open" | "2" =>
            if (state.isUnlocked) {
              println("Хранилище уже открыто. Сначала заблокируйте его командой '9'.")
            } else if (state.isBlockedPermanently) {
              println("Ошибка: Превышено максимальное количество попыток. Перезапустите приложение.")
            } else if (!new File(config.storageFile).exists()) {
              println("Ошибка: Файл хранилища не найден. Создайте новое хранилище командой '1'.")
            } else {
              handleOpen(config, state) match {
                case Some(newState) =>
                  state = newState
                  showCommands(isUnlocked = true)
                case None =>
              }
            }

          case "lock" | "9" =>
            if (state.isUnlocked) {
              state = state.copy(isUnlocked = false, masterPasswordPlain = "")
              Logger.log("INFO", "Хранилище заблокировано вручную")
              println("Хранилище заблокировано.")
              showCommands(isUnlocked = false)
            } else {
              println("Хранилище уже заблокировано.")
            }

          case "list" | "3" =>
            if (!checkAccess(state)) {
              // ничего не делаем
            } else {
              val sortedRecords = config.defaultSort match {
                case "service" => state.records.sortBy(_.service.toLowerCase)
                case "username" => state.records.sortBy(_.username.toLowerCase)
                case _ => state.records.sortBy(_.id)
              }
              println(f"ID   Сервис       Имя пользователя          Изменена")
              sortedRecords.foreach(r => println(f"${r.id}%-4s ${r.service}%-12s ${r.username}%-25s ${r.modified}"))
            }

          case cmd if cmd.startsWith("show ") || cmd.startsWith("4 ") =>
            val parts = cmd.split(" ")
            if (parts.length < 2) {
              println("Ошибка: Не указан ID записи. Пример: show 12")
            } else {
              val id = parts(1)
              state.records.find(_.id == id) match {
                case Some(r) =>
                  println(s"\nСервис: ${r.service}")
                  println(s"Адрес: ${r.url}")
                  println(s"Имя пользователя: ${r.username}")
                  println("Пароль: ********")
                  println(s"Заметка: ${r.note}")
                  println(s"Создана: ${r.created}")
                  println(s"Изменена: ${r.modified}")
                case None => println(s"Ошибка: Запись с ID $id не найдена.")
              }
            }

          case cmd if cmd.startsWith("pass ") =>
            val parts = cmd.split(" ")
            if (parts.length < 2) {
              println("Ошибка: Не указан ID записи. Пример: pass 12")
            } else {
              val id = parts(1)
              state.records.find(_.id == id) match {
                case Some(r) =>
                  println(s"Пароль: ${r.password}")
                  Logger.log("INFO", s"Отображён пароль записи: id=$id")
                case None => println(s"Ошибка: Запись с ID $id не найдена.")
              }
            }

          case "add" | "5" =>
            handleAdd(config, state) match {
              case Some(newState) => state = newState
              case None =>
            }

          case cmd if cmd.startsWith("edit ") || cmd.startsWith("6 ") =>
            val parts = cmd.split(" ")
            if (parts.length < 2) {
              println("Ошибка: Не указан ID записи. Пример: edit 12")
            } else {
              handleEdit(config, state, parts(1)) match {
                case Some(newState) => state = newState
                case None =>
              }
            }

          case cmd if cmd.startsWith("remove ") || cmd.startsWith("7 ") =>
            val parts = cmd.split(" ")
            if (parts.length < 2) {
              println("Ошибка: Не указан ID записи. Пример: remove 12")
            } else {
              handleRemove(config, state, parts(1)) match {
                case Some(newState) => state = newState
                case None =>
              }
            }

          case cmd if cmd.startsWith("search ") || cmd.startsWith("8 ") || cmd.startsWith("11 ") =>
            val query = cmd.substring(cmd.indexOf(" ") + 1).trim
            if (query.isEmpty) {
              println("Ошибка: Поисковый запрос не задан.")
            } else {
              val lowerQuery = query.toLowerCase
              val found = state.records.filter(r =>
                r.service.toLowerCase.contains(lowerQuery) ||
                  r.username.toLowerCase.contains(lowerQuery) ||
                  r.url.toLowerCase.contains(lowerQuery)
              )
              println(s"\nНайдено записей: ${found.size}")
              if (found.nonEmpty) {
                println(f"ID   Сервис   Имя пользователя")
                found.foreach(r => println(f"${r.id}%-4s ${r.service}%-8s ${r.username}"))
              }
              Logger.log("INFO", s"Выполнен поиск: найдено записей=${found.size}")
            }

          case "reload" | "10" =>
            if (state.isUnlocked) {
              println("Ошибка: Невозможно перезагрузить конфигурацию при открытом хранилище. Сначала заблокируйте его командой '9'.")
              Logger.log("WARN", "Попытка перезагрузки конфигурации при открытом хранилище")
            } else {
              ConfigManager.loadConfig("config.json") match {
                case Right(_) =>
                  println("Конфигурация успешно перезагружена.")
                  Logger.log("INFO", "Конфигурация перезагружена")
                case Left(err) =>
                  println(s"Ошибка перезагрузки: $err")
                  Logger.log("ERROR", s"Ошибка перезагрузки конфигурации: $err")
              }
            }

          case "" =>

          case _ =>
            println("Ошибка: Неизвестная команда. Введите 'help' для списка команд.")
            Logger.log("WARN", s"Неизвестная команда: $command")
        }
      }
    }
    Logger.close()
  }

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

  private def checkAccess(state: AppState): Boolean = {
    if (!state.isUnlocked) {
      println("Ошибка: Хранилище заблокировано. Введите '2' для открытия.")
      false
    } else {
      true
    }
  }

  private def handleCreate(config: AppConfig, state: AppState): Option[AppState] = {
    print("Введите новый мастер-пароль: ")
    val p1 = scala.io.StdIn.readLine()
    print("Повторите мастер-пароль: ")
    val p2 = scala.io.StdIn.readLine()

    if (p1 != p2) {
      println("Ошибка: Мастер-пароли не совпадают.")
      Logger.log("WARN", "Неудачная попытка создания хранилища: пароли не совпали")
      return None
    }
    if (p1.length < 4) {
      println("Ошибка: Пароль слишком короткий (минимум 4 символа).")
      return None
    }

    VaultManager.saveVault(config.storageFile, List.empty, p1) match {
      case Right(_) =>
        println("Хранилище успешно создано и открыто.")
        Logger.log("INFO", "Хранилище успешно создано")
        Some(state.copy(isUnlocked = true, failedAttempts = 0, masterPasswordPlain = p1))
      case Left(err) =>
        println(s"Ошибка создания: $err")
        Logger.log("ERROR", s"Ошибка создания хранилища: $err")
        None
    }
  }

  private def handleOpen(config: AppConfig, state: AppState): Option[AppState] = {
    print("Введите мастер-пароль: ")
    val input = scala.io.StdIn.readLine()

    VaultManager.loadVault(config.storageFile, input) match {
      case Right(records) =>
        println("Хранилище успешно открыто.")
        Logger.log("INFO", "Хранилище успешно открыто")
        Some(state.copy(isUnlocked = true, failedAttempts = 0, records = records, masterPasswordPlain = input))
      case Left(err) =>
        val newAttempts = state.failedAttempts + 1
        Logger.log("WARN", "Неудачная попытка открытия хранилища")
        println(s"Ошибка: $err. Осталось попыток: ${config.maximumUnlockAttempts - newAttempts}")

        if (newAttempts >= config.maximumUnlockAttempts) {
          println("Превышено максимальное количество попыток. Приложение заблокировано до перезапуска.")
          Logger.log("ERROR", "Превышено максимальное количество попыток открытия")
          Some(state.copy(isBlockedPermanently = true, failedAttempts = newAttempts))
        } else {
          Some(state.copy(failedAttempts = newAttempts))
        }
    }
  }

  private def handleAdd(config: AppConfig, state: AppState): Option[AppState] = {
    println("--- Добавление новой записи (введите 'отмена' для выхода) ---")
    print("Сервис (обязательно): ")
    val service = scala.io.StdIn.readLine().trim
    if (service.isEmpty || service.toLowerCase == "отмена") {
      println("Отменено.")
      return None
    }

    if (!state.isUnlocked) {
      println("Сессия истекла. Изменения не сохранены.")
      return None
    }

    print("Имя пользователя: ")
    val username = scala.io.StdIn.readLine().trim
    print("Пароль: ")
    val password = scala.io.StdIn.readLine().trim
    print("Email: ")
    val email = scala.io.StdIn.readLine().trim
    print("URL: ")
    val url = scala.io.StdIn.readLine().trim
    print("Заметка: ")
    val note = scala.io.StdIn.readLine().trim

    val now = LocalDateTime.now().format(dateFormat)
    val newRecord = Record(
      id = UUID.randomUUID().toString.substring(0, 8),
      service, username, password, email, url, note, now, now
    )

    val updatedRecords = state.records :+ newRecord
    VaultManager.saveVault(config.storageFile, updatedRecords, state.masterPasswordPlain) match {
      case Right(_) =>
        Logger.log("INFO", s"Добавлена запись: id=${newRecord.id}")
        println("Запись успешно добавлена.")
        Some(state.copy(records = updatedRecords))
      case Left(err) =>
        println(s"Ошибка сохранения: $err")
        Logger.log("ERROR", s"Неуспешное сохранение хранилища: $err")
        None
    }
  }

  private def handleEdit(config: AppConfig, state: AppState, id: String): Option[AppState] = {
    state.records.find(_.id == id) match {
      case Some(r) =>
        println(s"--- Изменение записи ID: $id (оставьте пустым, чтобы не менять, или 'отмена') ---")
        print(s"Сервис [${r.service}]: ")
        val service = scala.io.StdIn.readLine().trim
        if (service.toLowerCase == "отмена") {
          println("Отменено.")
          return None
        }

        if (!state.isUnlocked) {
          println("Сессия истекла. Изменения не сохранены.")
          return None
        }

        print(s"Имя пользователя [${r.username}]: ")
        val username = scala.io.StdIn.readLine().trim

        print("Изменить пароль? [да/нет]: ")
        val changePass = scala.io.StdIn.readLine().trim.toLowerCase
        val password = if (changePass == "да" || changePass == "yes") {
          print("Новый пароль: ")
          scala.io.StdIn.readLine().trim
        } else {
          r.password
        }

        print(s"Email [${r.email}]: ")
        val email = scala.io.StdIn.readLine().trim
        print(s"URL [${r.url}]: ")
        val url = scala.io.StdIn.readLine().trim
        print(s"Заметка [${r.note}]: ")
        val note = scala.io.StdIn.readLine().trim

        val updatedRecord = r.copy(
          service = if (service.nonEmpty) service else r.service,
          username = if (username.nonEmpty) username else r.username,
          password = password,
          email = if (email.nonEmpty) email else r.email,
          url = if (url.nonEmpty) url else r.url,
          note = if (note.nonEmpty) note else r.note
        ).updateModified()

        val updatedRecords = state.records.map(rec => if (rec.id == id) updatedRecord else rec)

        VaultManager.saveVault(config.storageFile, updatedRecords, state.masterPasswordPlain) match {
          case Right(_) =>
            Logger.log("INFO", s"Изменена запись: id=$id")
            println("Запись успешно изменена.")
            Some(state.copy(records = updatedRecords))
          case Left(err) =>
            println(s"Ошибка сохранения: $err")
            Logger.log("ERROR", s"Неуспешное сохранение при изменении: $err")
            None
        }
      case None =>
        println(s"Ошибка: Запись с ID $id не найдена.")
        None
    }
  }

  private def handleRemove(config: AppConfig, state: AppState, id: String): Option[AppState] = {
    state.records.find(_.id == id) match {
      case Some(r) =>
        if (config.confirmBeforeDelete) {
          println(s"\nБудет удалена запись:")
          println(s"Сервис: ${r.service}")
          println(s"Имя пользователя: ${r.username}")
          print("Удалить запись? [да/нет]: ")
          val ans = scala.io.StdIn.readLine().trim.toLowerCase
          if (ans != "да" && ans != "yes") {
            println("Удаление отменено.")
            return None
          }
        }

        val updatedRecords = state.records.filterNot(_.id == id)
        VaultManager.saveVault(config.storageFile, updatedRecords, state.masterPasswordPlain) match {
          case Right(_) =>
            Logger.log("INFO", s"Удалена запись: id=$id")
            println("Запись успешно удалена.")
            Some(state.copy(records = updatedRecords))
          case Left(err) =>
            println(s"Ошибка удаления: $err")
            Logger.log("ERROR", s"Неуспешное сохранение при удалении: $err")
            None
        }
      case None =>
        println(s"Ошибка: Запись с ID $id не найдена.")
        None
    }
  }
}