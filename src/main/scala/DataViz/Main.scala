package DataViz


object Main extends App {

  Data.loadData("src/main/scala/DataViz/data.json") match {
    case Some(d) => {
      println("x range: " + d.xRange)
      println("y range: " + d.yRange)
    }
    case _ => println("Fail")
  }


}
