import scala.io.StdIn.readLine

object ConsoleActionReader  extends ActionReader {
  override def apply(ask: String): String = readLine(ask)
}
