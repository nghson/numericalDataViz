package DataViz

import scalafx.Includes._
import scalafx.scene.shape._
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.canvas._
import scalafx.geometry.Pos._
import scalafx.scene.control.Label.sfxLabel2jfx


object Chart {
  /** Format Double number to either integer or double with 2 precision. */
  def fmt(n: Double): String = {
    if (n.isValidInt) n.toInt.toString
    else ((f"$n%1.2f"))
  }

  def make_line_chart(data: LineData, w: Double, h: Double, colors: Array[Color], config: Map[String, String]): Pane = {
    val canvas = new Canvas(w, h)
    val gt = canvas.graphicsContext2D
    val pane = new Pane
    pane.children += canvas
    canvas.layoutY = 40

    // axes
    val xAxis = Axis.get_x_axis(data, w, h)
    val yAxis = Axis.get_y_axis(data, w, h)
    val (xMarks, xMarkLabels) = Axis.get_x_marks(data, w, h, xAxis(0).y)
    val (yMarks, yMarkLabels) = Axis.get_y_marks(data, w, h, yAxis(0).x)

    gt.setStroke(Black)
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
          gt.setStroke(Gray)
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
      line.strokeWidth = 2
      val label = new Label(s"${data(0)}, ${data(1)}")
      hBox.children = List(line, label)
      hBox.alignment = CenterLeft
      hBox
    })
    vBox.alignment = Center

    vBox
  }

  def make_hist_chart(data: HistogramData, w: Double, h: Double, config: Map[String, String]): Pane = {
    val pane = new Pane
    val canvas = new Canvas(w, h)
    pane.children += canvas
    val yShift = 50 // shift canvas down
    val xShift = 50 // shift canvas rigt
    canvas.layoutY = yShift
    canvas.layoutX = xShift
    val gt = canvas.graphicsContext2D

    // axes
    gt.setStroke(Black)
    gt.lineWidth = 1
    gt.strokeLine(0, h, w, h)
    gt.strokeLine(0, 0, 0, h)
    val (yMarks, yMarkLabels) = Axis.get_hist_marks(data, w, h)
    yMarks.foreach(y => {
      gt.lineWidth = 0.5
      gt.setStroke(Gray)
      gt.strokeLine(0, y, w, y)
    })
    yMarkLabels.foreach(arr => {
      val n = arr(0)
      val y = arr(1)
      val label = new Label(fmt(n))
      pane.children += label
      label.layoutY = y + yShift
      label.layoutX = xShift/2
    })
    if (config.contains("y")) { // axis name
      val label = new Label(config("y"))
      label.layoutX = xShift
      label.layoutY = yShift/2
      pane.children += label
    }

    // plot
    val space = 20 // between each column
    val colWidth = (w - data.data.size*space)/data.data.size
    var current: Double = space
    data.data.foreach(p => {
      val y = (data.yRange._2 - p._2) * h / data.dimension
      gt.fill = Teal
      gt.fillRect(current, y, colWidth, h)
      val dataName = new Label(p._1)
      dataName.prefWidth = colWidth
      pane.children += dataName
      dataName.layoutY = h + yShift + 20
      dataName.layoutX = current + xShift + 10
      current += colWidth + space
    })
    pane
  }

  def make_pie_chart(data: PieData, width: Double, height: Double) = {
    val hBox = new HBox(30)
    val chart = new Pane
    val legend = new VBox(20)
    val colors = Array(Blue, Red, Green, Yellow, Coral, Teal, Aqua, Brown, Azure, Violet, Navy, Cyan)
    var total = data.pieData.values.toArray.sum
    // total = 0 make => change it to avoid 0 division, result should be the same
    if (total == 0) total = 1

    val centerX = width/2.8
    val centerY = height/2
    val radius = width/3.5
    var current = 0.0
    var i = 0

    // plot
    data.pieData.foreach(p => {
      val arcLength = p._2 / total * 360
      val arc = Arc(centerX, centerY, radius, radius, current, arcLength)
      val color = colors(i%colors.length)
      arc.setFill(color)
      arc.setType(ArcType.Round)
      chart.children += arc
      current += arcLength
      i += 1

      // legend
      val hBox = new HBox(10)
      val rect = Rectangle(10, 10, color)
      val label = new Label(p._1)
      val numberLabel = new Label(fmt(p._2))
      hBox.children = List(rect, label, numberLabel)
      hBox.alignment = CenterLeft
      legend.children += hBox
    })

    legend.alignment = Center
    hBox.children += chart
    hBox.children += legend
    //hBox.alignment = Center
    hBox
  }

}
