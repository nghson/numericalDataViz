package DataViz

class Point(val x: Double, val y: Double) {

  override def toString: String = "(" + Chart.fmt(x) + ", " + Chart.fmt(y) + ")"
}
