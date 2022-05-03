package DataViz

import scalafx.application._
import scalafx.Includes._
import scalafx.scene._
import scalafx.scene.layout._
import scalafx.geometry.Pos._
import scalafx.scene.paint.Color._

object Main {


  def main(args: Array[String]): Unit = {
    val data = Data.load_data("src/main/scala/DataViz/data.json") match {
      case Some(d) => d
      case _ => {
        println("Failed to load data.")
        return
      }
    }

    val w = 700
    val h = 400
    val colors = Array(Blue, Red, Green, Yellow, Coral, Teal, Aqua, Brown, Azure, Violet, Navy, Cyan)
    val config = data.config.getOrElse(Map[String, String]())

    val app = new JFXApp {
      stage = new JFXApp.PrimaryStage {
        title = {
          if (config.contains("name")) config("name")
          else "Graph"
        }
        scene = new Scene(w+400, h+100) {
          val hBox = new HBox(30)

          val pane = Chart.make_line_chart(data, w, h, colors, config)
          hBox.children += pane

          val vBox = Chart.make_legend(data, colors)
          hBox.children += vBox
          hBox.alignment = Center
          root = hBox
        }
      }
    }

    app.main(args)
  }

}
