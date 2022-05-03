package DataViz

import scalafx.application._
import scalafx.Includes._
import scalafx.scene._
import scalafx.scene.shape._
import scalafx.scene.paint.Color
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scalafx.scene.canvas._
import scalafx.geometry.Pos._
import math.abs

object Chart {

  def fmt(n: Double): String = {
    if (n == 0 || abs(n) > 1) n.toInt.toString
    else n.toString
  }

  def make_line_chart(data: LineData, w: Double, h: Double, colors: Array[Color]): Canvas = {
    val canvas = new Canvas(w, h)
    val gt = canvas.graphicsContext2D

    // axes
    val xAxis = Axis.get_x_axis(data, w, h)
    val yAxis = Axis.get_y_axis(data, w, h)
    val (xMarks, xMarkLabels) = Axis.get_x_marks(data, w, h, xAxis(0).y)
    val (yMarks, yMarkLabels) = Axis.get_y_marks(data, w, h, yAxis(0).x)

    gt.setStroke(Color.Black)
    gt.lineWidth = 1.0
    //gt.fill = Color.Black

    gt.strokeLine(xAxis(0).x, xAxis(0).y, xAxis(1).x, xAxis(1).y)
    xMarks.foreach(line => gt.strokeLine(line(0).x, line(0).y, line(1).x, line(1).y))
    xMarkLabels.foreach(label => {
      // avoid "0" label
      if (label(0) != 0)
        gt.fillText(fmt(label(0)), label(1), label(2))
    })
    //xMarkLabels.foreach(label => println(label(0), label(1), label(2)))

    gt.strokeLine(yAxis(0).x, yAxis(0).y, yAxis(1).x, yAxis(1).y)
    yMarks.foreach(line => gt.strokeLine(line(0).x, line(0).y, line(1).x, line(1).y))
    yMarkLabels.foreach(label => {
      // avoid "0" label
      if (label(0) != 0)
        gt.fillText(fmt(label(0)), label(1), label(2))
    })
    //yMarkLabels.foreach(label => println(label(0), label(1), label(2)))

    // plot
    gt.lineWidth = 2.5
    var i = 0

    data.get_transformed_data(w, h).foreach(line => {
      val start = line(0)
      val end = line(1)

      //println(s"(${start.x}, ${start.y}), (${end.x}, ${end.y})")

      gt.setStroke(colors(i))
      gt.strokeLine(start.x, start.y, end.x, end.y)
      i += 1
    })

    // grid
    val grid = new Grid(10, w, h, data)
    val gridV = grid.verticals
    val gridH = grid.horizontals

    gt.lineWidth = 0.5
    gt.setStroke(Color.Gray)
    gt.setLineDashes(5)
    gridV.foreach(line => gt.strokeLine(line(0).x, line(0).y, line(1).x, line(1).y))
    gridH.foreach(line => gt.strokeLine(line(0).x, line(0).y, line(1).x, line(1).y))
    //gridV.foreach(line => println(line(0).x, line(0).y, line(1).x, line(1).y))

    canvas
  }

  def make_legend(data: LineData, colors: Array[Color]): VBox = {
    val legends = scala.collection.mutable.ArrayBuffer[(Color, Array[Point])]()
    for (i <- data.data.indices) {
      legends += ((colors(i), data.data(i)))
    }
    val vBox = new VBox(20)
    vBox.children = legends.map(l => {
      val col = l._1
      val data = l._2
      val hBox = new HBox(10)
      val line = Line(10, 5, 30, 5)
      line.stroke = col
      val label = new Label(s"${data(0)}, ${data(1)}")
      hBox.children = List(line, label)
      hBox
    })
    vBox.alignment = Center

    vBox
  }

}
