package DataViz

import math._

class Axis(val data: LineData, val width: Double, val height: Double) {

  // return axis as two points
  def get_x_axis: Array[Point] = {
    if (data.yRange._1 >= 0) {
      Array(new Point(0, height), new Point(width, height))
    } else if (data.yRange._2 < 0) {
      Array(new Point(0, 0), new Point(width, 0))
    } else {
      val scaleY = height / data.dimensions._2
      Array(new Point(0, data.yRange._2 * scaleY), new Point(width, data.yRange._2 * scaleY))
    }
  }

  def get_y_axis: Array[Point] = {
    if (data.xRange._1 >= 0) {
      Array(new Point(0, 0), new Point(0, height))
    } else if (data.xRange._2 < 0) {
      Array(new Point(width, 0), new Point(width, height))
    } else {
      val scaleX = width / data.dimensions._1
      Array(new Point(-data.xRange._1 * scaleX, 0), new Point(-data.xRange._1 * scaleX, height))
    }
  }

  // return the marks on axis
  def get_x_marks: (Array[Array[Point]], Array[Array[Double]]) = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val arr2 = scala.collection.mutable.ArrayBuffer[Array[Double]]()

    val y = get_x_axis(0).y // y and yLenght are already scaled
    val yLength = { // length of the mark
      if (y == 0) y + 5
      else y - 5
    }
    val yLabel = { // y position for the mark labels
      if (y == 0) y + 8
      else y - 8
    }

    if (data.dimensions._1 > 0) {
      val scaleX = width / data.dimensions._1
      val unit = pow(10, floor(log10(data.dimensions._1)))

      var current = 0.0
      if (data.xRange._1 >= 0) {
        while (current <= data.xRange._2) {
          if (current >= data.xRange._1) {
            arr +=
              Array(new Point((current - data.xRange._1) * scaleX, y), new Point((current - data.xRange._1) * scaleX, yLength))
            arr2 +=
              Array(current, (current - data.xRange._1) * scaleX, yLabel)
          }
          current += unit
        }
      } else if (data.xRange._2 < 0) {
        while (current >= data.xRange._1) {
          if (current <= data.xRange._2) {
            arr +=
              Array(new Point((current - data.xRange._1) * scaleX, y), new Point((current - data.xRange._1) * scaleX, yLength))
            arr2 +=
              Array(current, (current - data.xRange._1) * scaleX, yLabel)
          }
          current -= unit
        }
      } else {
        while (current <= data.xRange._2) {
          arr +=
            Array(new Point((current - data.xRange._1) * scaleX, y), new Point((current - data.xRange._1) * scaleX, yLength))
          arr2 +=
            Array(current, (current - data.xRange._1) * scaleX, yLabel)
          current += unit
        }
        current = 0
        while (current >= data.xRange._1) {
          arr +=
            Array(new Point((current - data.xRange._1) * scaleX, y), new Point((current - data.xRange._1) * scaleX, yLength))
          arr2 +=
            Array(current, (current - data.xRange._1) * scaleX, yLabel)
          current -= unit
        }
      }
    } else { // plotting a vertical line or a point
      arr += Array(new Point(width/2, y), new Point(width/2, yLength))
      arr2 += Array(data.xRange._1, width/2, yLabel)
    }

    (arr.toArray, arr2.toArray)
  }

  def get_y_marks: (Array[Array[Point]], Array[Array[Double]]) = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val arr2 = scala.collection.mutable.ArrayBuffer[Array[Double]]()

    val x = get_y_axis(0).x // y and yLenght are already scaled
    val xLength = { // length of the mark
      if (x == width) x - 5
      else x + 5
    }
    val xLabel = { // x position for the mark labels
      if (x == width) x - 8
      else x + 8
    }

    if (data.dimensions._2 > 0) {
      val scaleY = height / data.dimensions._2
      val unit = pow(10, floor(log10(data.dimensions._2)))

      var current = 0.0
      if (data.yRange._1 >= 0) {
        while (current <= data.yRange._2) {
          if (current >= data.yRange._1) {
            arr +=
              Array(new Point(x, (-current+data.yRange._2)*scaleY), new Point(xLength, (-current + data.yRange._2) * scaleY))
            arr2 +=
              Array(current, xLabel, (-current + data.yRange._2) * scaleY)
          }
          current += unit
        }
      } else if (data.yRange._2 < 0) {
        while (current >= data.yRange._1) {
          if (current <= data.yRange._2) {
            arr +=
              Array(new Point(x, (-current+data.yRange._2)*scaleY), new Point(xLength, (-current + data.yRange._2) * scaleY))
            arr2 +=
              Array(current, xLabel, (-current + data.yRange._2) * scaleY)
          }
          current -= unit
        }
      } else {
        while (current <= data.yRange._2) {
          arr +=
            Array(new Point(x, (-current + data.yRange._2) * scaleY), new Point(xLength, (-current + data.yRange._2) * scaleY))
          arr2 +=
            Array(current, xLabel, (-current + data.yRange._2) * scaleY)
          current += unit
        }
        current = 0
        while (current >= data.yRange._1) {
          arr +=
            Array(new Point(x, (-current + data.yRange._2) * scaleY), new Point(xLength, (-current + data.yRange._2) * scaleY))
          arr2 +=
            Array(current, xLabel, (-current + data.yRange._2) * scaleY)
          current -= unit
        }
      }
    } else { // plotting a horizontal line or a point
      arr += Array(new Point(x, height/2), new Point(xLength, height/2))
      arr2 += Array(data.xRange._2, xLabel, height/2)
    }

    (arr.toArray, arr2.toArray)
  }
}
