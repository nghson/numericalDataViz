import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Line
import scalafx.scene.layout.Pane

object Main extends JFXApp {

  stage = new JFXApp.PrimaryStage {
    title.value = "Hello"
    width = 800
    height = 400
  }

  val root = new Pane //Simple pane component
  val scene = new Scene(root) //Scene acts as a container for the scene graph
  stage.scene = scene

  // generate random points and lines
  val r = scala.util.Random
  def makePoints = (r.nextDouble()*600 + 100, r.nextDouble()*200 + 100)

  val lineList = Vector.fill(10)( (makePoints, makePoints) )
  for (line <- lineList) {
    val point1 = line._1
    val point2 = line._2
    val toBeAdded = new Line {
      startX = point1._1
      startY = point1._2
      endX = point2._1
      endY = point2._2
    }
    root.children += toBeAdded
  }

}
