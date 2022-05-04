package DataViz

object Grid {
  /** Get vertical lines in the grid.
   *  @param size grid size
   *  @param x x-coordinate of the y-axis */
  def verticals(size: Double, width: Double, height: Double, data: LineData, x: Double): Array[Array[Point]] = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val scaleX = width / data.dimensions._1

    var current = size
    while (current <= data.xRange._2) {
      if (current >= data.xRange._1) {
        arr +=
          Array(new Point((current - data.xRange._1) * scaleX, 0), new Point((current - data.xRange._1) * scaleX, height))
      }
      current += size
    }
    current = -size
    while (current >= data.xRange._1) {
      if (current <= data.xRange._2) {
        arr +=
          Array(new Point((current - data.xRange._1) * scaleX, 0), new Point((current - data.xRange._1) * scaleX, height))
      }
      current -= size
    }

    arr.toArray
  }

  /** Get horizontal lines in the grid.
   *  @param y y-coordinate of the x-axis */
  def horizontals(size: Double, width: Double, height: Double, data: LineData, y: Double): Array[Array[Point]] = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val scaleY = height / data.dimensions._2

    var current = size
    while (current <= data.yRange._2) {
      if (current >= data.yRange._1) {
        arr +=
          Array(new Point(0, (data.yRange._2 - current) * scaleY), new Point(width, (data.yRange._2 - current) * scaleY))
      }
      current += size
    }
    current = -size
    while (current >= data.yRange._1) {
      if (current <= data.yRange._2) {
        arr +=
          Array(new Point(0, (data.yRange._2 - current) * scaleY), new Point(width, (data.yRange._2 - current) * scaleY))
      }
      current -= size
    }

    arr.toArray
  }
}
