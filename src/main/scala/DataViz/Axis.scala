package DataViz

import math._

object Axis {

  // return axis as two points
  def get_x_axis(data: LineData, width: Double, height: Double): Array[Point] = {
    if (data.yRange._1 >= 0) {
      Array(new Point(0, height), new Point(width, height))
    } else if (data.yRange._2 < 0) {
      Array(new Point(0, 0), new Point(width, 0))
    } else {
      val scaleY = height / data.dimensions._2
      Array(new Point(0, data.yRange._2 * scaleY), new Point(width, data.yRange._2 * scaleY))
    }
  }

  def get_y_axis(data: LineData, width: Double, height: Double): Array[Point] = {
    if (data.xRange._1 >= 0) {
      Array(new Point(0, 0), new Point(0, height))
    } else if (data.xRange._2 < 0) {
      Array(new Point(width, 0), new Point(width, height))
    } else {
      val scaleX = width / data.dimensions._1
      Array(new Point(-data.xRange._1 * scaleX, 0), new Point(-data.xRange._1 * scaleX, height))
    }
  }

  /** Calculate the positions of the marks on x-axis.
   *  @param y the y-coordinate of the x-axis.
   */
  def get_x_marks(data: LineData, width: Double, height: Double, y: Double): (Array[Array[Point]], Array[Array[Double]]) = {
    // coordinates of the marks so we need two points
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    //coordinates of the number label so only need one point
    val arr2 = scala.collection.mutable.ArrayBuffer[Array[Double]]()

    val yLength = { // length of the mark
      if (y == 0) y + 5
      else y - 5
    }
    val yLabel = { // y position for the mark labels
      if (y == 0) y + 15
      else y - 15
    }

    val scaleX = width / data.dimensions._1
    var unit = pow(10, floor(log10(data.dimensions._1)))
    if (data.dimensions._1 <= unit * 2) unit = unit/4
    else if (data.dimensions._1 <= unit * 5) unit = unit/2
    else if (data.dimensions._1 >= unit * 7) unit = unit*2

    var current = unit
    while (current <= data.xRange._2) {
      if (current >= data.xRange._1) {
        arr +=
          Array(new Point((current - data.xRange._1) * scaleX, y), new Point((current - data.xRange._1) * scaleX, yLength))
        arr2 +=
          Array(current, (current - data.xRange._1) * scaleX, yLabel)
      }
      current += unit
    }
    current = -unit
    while (current >= data.xRange._1) {
      if (current <= data.xRange._2) {
        arr +=
          Array(new Point((current - data.xRange._1) * scaleX, y), new Point((current - data.xRange._1) * scaleX, yLength))
        arr2 +=
          Array(current, (current - data.xRange._1) * scaleX, yLabel)
      }
      current -= unit
    }

    (arr.toArray, arr2.toArray)
  }

  /** Calculate the positions of the marks on y-axis.
   *  @param x the x-coordinate of the y-axis.
   */
  def get_y_marks(data: LineData, width: Double, height: Double, x: Double): (Array[Array[Point]], Array[Array[Double]]) = {
    // coordinates of the marks so we need two points
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    //coordinates of the number label so only need one point
    val arr2 = scala.collection.mutable.ArrayBuffer[Array[Double]]()

    val xLength = { // length of the mark
      if (x == width) x - 5
      else x + 5
    }
    val xLabel = { // x position for the mark labels
      if (x >= width - 50) x - 25
      else x + 15
    }

    val scaleY = height / data.dimensions._2
    var unit = pow(10, floor(log10(data.dimensions._2)))
    if (data.dimensions._2 <= unit * 2) unit = unit/4
    else if (data.dimensions._2 <= unit * 5) unit = unit/2
    else if (data.dimensions._2 >= unit * 7) unit = unit * 2

    var current = unit
    while (current <= data.yRange._2) {
      if (current >= data.yRange._1) {
        arr +=
          Array(new Point(x, (-current+data.yRange._2)*scaleY), new Point(xLength, (-current + data.yRange._2) * scaleY))
        arr2 +=
          Array(current, xLabel, (-current + data.yRange._2) * scaleY)
      }
      current += unit
    }
    current = -unit
    while (current >= data.yRange._1) {
      if (current <= data.yRange._2) {
        arr +=
          Array(new Point(x, (-current+data.yRange._2)*scaleY), new Point(xLength, (-current + data.yRange._2) * scaleY))
        arr2 +=
          Array(current, xLabel, (-current + data.yRange._2) * scaleY)
      }
      current -= unit
    }

    (arr.toArray, arr2.toArray)
  }
}
