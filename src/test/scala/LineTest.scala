import DataViz.{LineData, Point}
import org.scalatest.flatspec.AnyFlatSpec

class LineTest extends AnyFlatSpec {
  "Data" should "pad ranges" in {
    val data = new LineData(Array(Array(new Point(1.0, 1), new Point(1, 1))))
    assert(data.dimensions._1 != 0)
    assert(data.dimensions._2 != 0)
    assert(data.dimensions._1 == 0.5)
    assert(data.dimensions._2 == 0.5)
  }

}
