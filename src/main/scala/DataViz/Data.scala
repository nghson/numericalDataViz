package DataViz

import math.{pow, floor, log10, abs}
import io.circe.generic.auto._
import io.circe.parser.decode


case class LineData(val data: Array[Array[Point]],
                    val config: Option[Map[String, String]]=None) {

  val xRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.x)
    // padding
    val unit = {
      if (arr.min != arr.max)
        pow(10, floor(log10(arr.max - arr.min))) / 4
      else if (arr.min != 0)
        abs(arr.min)/4
      else
        10
    }
    (arr.min - unit, arr.max + unit)
  }

  val yRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.y)
    // padding
    val unit = {
      if (arr.min != arr.max)
        pow(10, floor(log10(arr.max - arr.min))) / 4
      else if (arr.min != 0)
        abs(arr.min)/4
      else
        10
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

}

case class HistogramData(val data: Map[String, Double],
                         val config: Option[Map[String, String]]=None) {
  val yRange: (Double, Double) = {
    val arr = this.data.values.toArray
    // padding
    val unit = {
      if (arr.max != 0)
        pow(10, floor(log10(arr.max - arr.min))) / 4
      else
        10
    }
    (0, arr.max + unit)
  }

  val dimension = yRange._2 - yRange._1

}

case class PieData(val pieData: Map[String, Double],  val config: Option[Map[String, String]]=None) {

}

object Data {

  def load_data(file: String): Option[LineData] = {
    val fileSource = util.Try(scala.io.Source.fromFile(file))

    def read_text(source: scala.io.Source) = source.getLines().mkString("\n")

    for (source <- fileSource) {
      val text = read_text(source)
      (decode[LineData](text)) match {
        case Right(data) => return Some(data)
        case Left(_) =>  return None
      }
    }

    None
  }

  def load_data_hist(file: String): Option[HistogramData] = {
    val fileSource = util.Try(scala.io.Source.fromFile(file))

    def read_text(source: scala.io.Source) = source.getLines().mkString("\n")

    for (source <- fileSource) {
      val text = read_text(source)
      (decode[HistogramData](text)) match {
        case Right(data) => return Some(data)
        case Left(_) =>  return None
      }
    }

    None
  }

  def load_data_pie(file: String): Option[PieData] = {
    val fileSource = util.Try(scala.io.Source.fromFile(file))

    def read_text(source: scala.io.Source) = source.getLines().mkString("\n")

    for (source <- fileSource) {
      val text = read_text(source)
      (decode[PieData](text)) match {
        case Right(data) => return Some(data)
        case Left(_) =>  return None
      }
    }

    None
  }
}
