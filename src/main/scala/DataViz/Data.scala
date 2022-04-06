package DataViz

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Line

class Data(val graphType: String, val config: Option[Map[String, String]]) {


}

class LineData(val data: Array[Array[Point]],
               graphType: String,
               config: Option[Map[String, String]]=None) extends Data(graphType, config) {

  val xRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.x)
    (arr.min, arr.max)
  }

  val yRange: (Double, Double) = {
    val arr = this.data.flatten.map(_.y)
    (arr.min, arr.max)
  }

}
