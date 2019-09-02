import Interpreter._

import scala.io.Source

object App {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(args(0))
    val s = source.getLines.mkString
    source.close()

    eval(s)
  }
}
