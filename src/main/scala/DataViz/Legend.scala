package DataViz

import scalafx.scene.paint.Color

class Legend {
  private val legends = scala.collection.mutable.ArrayBuffer[(Color, Array[Point])]()

  def add_legend(col: Color,  data: Array[Point]) = this.legends += ((col, data))

  def get_legend = this.legends.toArray

}
