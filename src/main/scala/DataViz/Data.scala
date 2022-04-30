package DataViz

import math.pow
import math.sqrt
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser.decode

abstract class Data(graphType: String, config: Option[Map[String, String]]=None) {

}


case class LineData(val data: Array[Array[Point]],
                    val graphType: String,
                    val config: Option[Map[String, String]]=None) extends Data(graphType, config) {

  val xRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.x)
    (arr.min, arr.max)
  }

  val yRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.y)
    (arr.min, arr.max)
  }

  val dimensions = (this.xRange._2 - this.xRange._1, this.yRange._2 - this.yRange._1)

  // transform coordinates of points to fit on canvas of size width * height
  def get_transformed_data(width: Double, height: Double): Array[Array[Point]] = {

    var scaleX = 1.0
    var scaleY = 1.0
    if (dimensions._1 != 0) {
      scaleX = width/dimensions._1
    }
    if (dimensions._2 != 0) {
       scaleY = height/dimensions._2
    }

    if (dimensions._1 == 0 && dimensions._2 == 0) { // plot 1 point
      this.data.map(line => {
        line.map(p => new Point(width/2, height/2))
      })
    } else if (dimensions._1 == 0) { // plot a vertical line
      this.data.map(line => {
        line.map(p => new Point(width/2, (this.yRange._2 - p.y) * scaleY))
      })
    } else if (dimensions._2 == 0) { //plot a horizontal line
      this.data.map(line => {
        line.map(p => new Point((p.x - this.xRange._1) * scaleX, height/2))
      })
    } else { // no dimension is 0
      this.data.map(line => {
        line.map(p => new Point((p.x - this.xRange._1) * scaleX, (this.yRange._2 - p.y) * scaleY))
      })
    }

  }

  // L2 distance for debug
  def lengths = {
    def l2(p1: Point, p2: Point) = sqrt( pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2) )
    this.data.map(line => l2(line(0), line(1)))
  }

}

object Data {

  def load_data(file: String): Option[LineData] = {
    val fileSource = util.Try(scala.io.Source.fromFile(file))

    def read_text(source: scala.io.Source) = source.getLines().mkString("\n")

    for (source <- fileSource) {
      val text = read_text(source)
      (decode[LineData](text)) match {
        case Right(data) => return Some(data)
        case Left(_) => return None
      }
    }

    None

  }

}
