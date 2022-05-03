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
import scalafx.scene.control.Label.sfxLabel2jfx


object Chart {

  def fmt(n: Double): String = {
    if (n.isValidInt) n.toInt.toString
    else ((f"$n%1.1f"))
  }

  def make_line_chart(data: LineData, w: Double, h: Double, colors: Array[Color], config: Map[String, String]): Pane = {
    val canvas = new Canvas(w, h)
    val gt = canvas.graphicsContext2D
    val pane = new Pane
    pane.children += canvas

    // axes
    val xAxis = Axis.get_x_axis(data, w, h)
    val yAxis = Axis.get_y_axis(data, w, h)
    val (xMarks, xMarkLabels) = Axis.get_x_marks(data, w, h, xAxis(0).y)
    val (yMarks, yMarkLabels) = Axis.get_y_marks(data, w, h, yAxis(0).x)

    gt.setStroke(Color.Black)
    gt.lineWidth = 0.6

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

    // axes name
    if (config.contains("y")) {
      val label = new Label(config("y"))
      label.layoutX = yAxis(0).x
      label.layoutY = 10
      canvas.layoutY = 40
      pane.children += label
    }
    if (config.contains("x")) {
      val label = new Label(config("x"))
      label.layoutX = w + 20
      label.layoutY = xAxis(0).y + 40
      pane.children += label
    }

    // plot
    gt.lineWidth = 2.5
    var i = 0
    data.get_transformed_data(w, h).foreach(line => {
      val start = line(0)
      val end = line(1)
      gt.setStroke(colors(i%colors.length))
      gt.strokeLine(start.x, start.y, end.x, end.y)
      i += 1
    })

    // grid
    if (config.contains("grid")) {
      val gridSize = util.Try(config("grid").toDouble)
      for (size <- gridSize) {
        if (size > 0) {
          val gridV = Grid.verticals(size, w, h, data, yAxis(0).x)
          val gridH = Grid.horizontals(size, w, h, data, xAxis(0).y)

          gt.lineWidth = 0.5
          gt.setStroke(Color.Gray)
          gt.setLineDashes(5)
          gridV.foreach(line => gt.strokeLine(line(0).x, line(0).y, line(1).x, line(1).y))
          gridH.foreach(line => gt.strokeLine(line(0).x, line(0).y, line(1).x, line(1).y))
        }
      }
    }

    pane
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
      val line = Line(10, 5, 20, 5)
      line.stroke = col
      val label = new Label(s"${data(0)}, ${data(1)}")
      hBox.children = List(line, label)
      hBox.alignment = CenterLeft
      hBox
    })
    vBox.alignment = Center

    vBox
  }

}
