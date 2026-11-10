
sealed trait Command

object Command {
  
  case object Create extends Command

  case object Open extends Command

  case object List extends Command

  case object Add extends Command

  case object Lock extends Command

  case object Reload extends Command

  case object Exit extends Command

  case object Help extends Command
  
  case class Show(id: String) extends Command

  case class Pass(id: String) extends Command

  case class Edit(id: String) extends Command

  case class Remove(id: String) extends Command

  case class Search(query: String) extends Command
  
  case class Unknown(text: String) extends Command
}