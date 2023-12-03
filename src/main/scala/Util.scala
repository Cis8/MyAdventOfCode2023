import scala.io.Source
import scala.util.Using

object Util {

  def parse[T](filename: String)(f: String => T): Seq[T] = {
    Using(Source.fromFile(s"./src/main/resources/$filename")) {
      src =>
        src.getLines()
          .map(f)
          .toList
    }.get
  }
}
