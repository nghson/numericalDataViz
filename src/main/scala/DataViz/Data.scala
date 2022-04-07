package DataViz

import io.circe.generic.auto._, io.circe.syntax._, io.circe.parser.decode

class Data(val graphType: String, val config: Option[Map[String, String]]=None) { //val config: Option[Map[String, String]]) {


}


class LineData(val data: Array[Array[Point]],
               val graphType: String,
               val config: Option[Map[String, String]]=None) {

  val xRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.x)
    (arr.min, arr.max)
  }

  val yRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.y)
    (arr.min, arr.max)
  }

}

object Data {
  def loadData(file: String): Option[LineData] = {
    val fileSource = util.Try(scala.io.Source.fromFile(file))


    def readText(source: scala.io.Source ) = source.getLines().mkString("\n")

    for (source <- fileSource) {
      val text = readText(source)
      (decode[LineData](text)) match {
        case Right(data) => return Some(data)
        case Left(_) => return None
      }
    }

    None

  }
}
