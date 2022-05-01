package DataViz

class Grid(val size: Double, val width: Double, val height: Double, val data: LineData) {
  val verticals: Array[Array[Point]] = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val dimensions: (Double, Double) = {
      if (data.dimensions._1 > 0) data.dimensions
      else (100, data.dimensions._2)
    }
    val scaleX = width / dimensions._1
    var current = size
    while (current <= dimensions._1) {
      arr += Array(new Point(current * scaleX, 0), new Point(current * scaleX, height))
      current += size
    }
    arr.toArray
  }

  val horizontals: Array[Array[Point]] = {
    val arr = scala.collection.mutable.ArrayBuffer[Array[Point]]()
    val dimensions: (Double, Double) = {
      if (data.dimensions._2 > 0) data.dimensions
      else (data.dimensions._1, 100)
    }
    val scaleY = height / dimensions._2
    var current = size
    while (current <= dimensions._2) {
      arr += Array(new Point(0, current * scaleY), new Point(width, current * scaleY))
      current += size
    }
    arr.toArray
  }
}
