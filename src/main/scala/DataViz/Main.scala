package DataViz

import scalafx.application._
import scalafx.Includes._
import scalafx.scene._
import scalafx.scene.layout._
import scalafx.geometry.Pos._
import scalafx.scene.paint.Color._

object Main {


  def main(args: Array[String]): Unit = {
    //val filename = "src/main/scala/DataViz/data.json"
    //val filename = "src/main/scala/DataViz/data_hist.json"
    val filename = "src/main/scala/DataViz/data_pie.json"


    def line(data: LineData) = {
      // plot size
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

    def hist(data: HistogramData) = {
      // plot size
      val w = 900
      val h = 400
      val config = data.config.getOrElse(Map[String, String]())

      val app = new JFXApp {
        stage = new JFXApp.PrimaryStage {
          title = {
            if (config.contains("name")) config("name")
            else "Graph"
          }
          scene = new Scene(w+100, h+200) {
            val pane = Chart.make_hist_chart(data, w, h, config)

            root = pane
          }
        }
      }
      app.main(args)

    }

    def pie(data: PieData): Unit = {
      // check if data >= 0
      data.pieData.foreach(p => {
        if (p._2 < 0) {
          println(s"Value ${p._2} < 0. Please use non-negative values only.")
          return
        }
      })
      // plot size
      val w = 600
      val h = 500

      val config = data.config.getOrElse(Map[String, String]())

      val app = new JFXApp {
        stage = new JFXApp.PrimaryStage {
          title = {
            if (config.contains("name")) config("name")
            else "Graph"
          }
          scene = new Scene(w, h) {
            val pane = Chart.make_pie_chart(data, w, h)

            root = pane
          }
        }
      }
      app.main(args)

    }

    Data.load_data(filename) match {
      case Some(d) => {
        println("Successfully loaded LineData")
        line(d)
        return
      }
      case _ => {
        println(".")
      }
    }

    Data.load_data_hist(filename) match {
      case Some(d) => {
        println("Successfully loaded HistogramData")
        hist(d)
        return
      }
      case _ => {
        println(".")
      }
    }

    Data.load_data_pie(filename) match {
      case Some(d) => {
        println("Successfully loaded PieData")
        pie(d)
      }
      case _ => {
        println(".")
      }
    }

  }

}
