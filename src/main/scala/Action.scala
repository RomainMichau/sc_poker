
object Action {

  private def readAmount()(using actionReader: ActionReader): Int = actionReader("amount: ").toIntOption.getOrElse(readAmount())

  def readAction()(using actionReader: ActionReader): Action = {
    val action = actionReader("pick action: ").toIntOption.getOrElse(readAction())
    action match {
      case 1 => Fold()
      case 2 => Call()
      case 3 =>
        val amount = readAmount()
        Raise(amount)
      case _ =>
        println("invalid action")
        readAction()

    }
  }

}

sealed trait Action

case class Fold() extends Action

case class Call() extends Action

case class Raise(raise: Int) extends Action