package calc

import scala.util.{Try, Failure, Success}

private[calc] object Implicits {

  implicit class OptionToTry[A](o: Option[A]){
    def toTry(e: Throwable): Try[A] =
      o.fold[Try[A]](Failure(e))(Success(_))
  }

}
