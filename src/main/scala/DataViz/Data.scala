package DataViz

import math.{pow, sqrt, floor, log10, abs}
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser.decode

abstract class Data(graphType: String, config: Option[Map[String, String]]=None) {

}

case class LineData(val data: Array[Array[Point]],
                    val graphType: String = "line",
                    val config: Option[Map[String, String]]=None) extends Data(graphType, config) {

  val xRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.x)
    // padding
    val unit = {
      if (arr.min != arr.max)
        pow(10, floor(log10(arr.max - arr.min))) / 4
      else
        abs(arr.min)/4
    }
    (arr.min - unit, arr.max + unit)
  }

  val yRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.y)
    // padding
    val unit = {
      if (arr.min != arr.max)
        pow(10, floor(log10(arr.max - arr.min))) / 4
      else
        abs(arr.min)/4
    }
    (arr.min - unit, arr.max + unit)
  }

  // dimensions with padding are never 0 so we can safely divide
  val dimensions = (this.xRange._2 - this.xRange._1, this.yRange._2 - this.yRange._1)

  // transform coordinates of points to fit on canvas of size width * height
  def get_transformed_data(width: Double, height: Double): Array[Array[Point]] = {

    val scaleX = width/dimensions._1
    val scaleY = height/dimensions._2

    this.data.map(line => {
      line.map(p => new Point((p.x - this.xRange._1) * scaleX, (this.yRange._2 - p.y) * scaleY))
    })

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
