package DataViz

object Grid {
  def verticals(size: Double, width: Double, height: Double, data: LineData, x: Double): Array[Array[Point]] = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val dimensions: (Double, Double) = data.dimensions
    val scaleX = width / dimensions._1
    val scaledSize = size * scaleX
    var current = x + scaledSize
    while (current <= width) {
      arr += Array(new Point(current, 0), new Point(current, height))
      current += scaledSize
    }
    current = x - scaledSize
    while (current >= 0) {
      arr += Array(new Point(current, 0), new Point(current, height))
      current -= scaledSize
    }
    arr.toArray
  }

  def horizontals(size: Double, width: Double, height: Double, data: LineData, y: Double): Array[Array[Point]] = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val dimensions: (Double, Double) = data.dimensions
    val scaleY = height / dimensions._2
    val scaledSize = size * scaleY
    var current = y + scaledSize
    while (current <= height) {
      arr += Array(new Point(0, current), new Point(width, current))
      current += scaledSize
    }
    current = y - scaledSize
    while (current >= 0) {
      arr += Array(new Point(0, current), new Point(width, current))
      current -= scaledSize
    }
    arr.toArray
  }
}
