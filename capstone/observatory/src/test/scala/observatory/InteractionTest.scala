package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import observatory.Interaction._

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  val colorScale = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  test("tile location") {
    println(tileLocation(0, 0, 0))
    println(tileLocation(0, 1, 1))
    println(tileLocation(0, 1, 0))
    println(tileLocation(1, 1, 1))
    println(tileLocation(2, 3, 3))
    println(tileLocation(3, 7, 7))
  }

  test("tile") {
    val avgTemperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv"))
//    tile(avgTemperatures, colorScale, 1, 0, 0).output("target/temperatures/2015/1/0-0.png")
//    tile(avgTemperatures, colorScale, 1, 0, 1).output("target/temperatures/2015/1/0-1.png")
//    tile(avgTemperatures, colorScale, 1, 1, 0).output("target/temperatures/2015/1/1-0.png")
//    tile(avgTemperatures, colorScale, 1, 1, 1).output("target/temperatures/2015/1/1-1.png")

//    println(2)
//    for (x <- 0 to 3; y <- 0 to 3) {
//      println((x, y))
//      tile(avgTemperatures, colorScale, 2, x, y).output(s"target/temperatures/2015/2/$x-$y.png")
//    }
    println(3)
    for (x <- 0 to 7; y <- 0 to 7) {
      println((x, y))
      tile(avgTemperatures, colorScale, 3, x, y).output(s"target/temperatures/2015/3/$x-$y.png")
    }



  }


}
