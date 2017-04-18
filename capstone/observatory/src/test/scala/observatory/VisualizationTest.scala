package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import observatory.Visualization._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

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

  test("circle distance") {
    val loc1 = Location(52.52, 13.405)
    val loc2 = Location(46.05695, 14.505751)

    assert(circleDistance(loc1, loc1) == 0)
    assert(circleDistance(loc2, loc2) == 0)
    assert(circleDistance(loc1, loc2) == circleDistance(loc2, loc1))
    assert(math.abs(circleDistance(loc1, loc2) - 724) < 1)

    assert(math.abs(circleDistance(Location(0, 0), Location(0, 90)) - 10000) < 10)
    assert(math.abs(circleDistance(Location(0, 0), Location(90, 0)) - 10000) < 10)

    assert(math.abs(circleDistance(Location(90, 0), Location(-90, 0)) - 20000) < 20)

    val rio = Location(-22.970722, -43.182365)
    val moscow = Location(55.644466, 37.395744)
    assert(math.abs(circleDistance(moscow, rio) - 11545) < 20)
  }

  test("predict temperature") {
    val smallSquare = List(
      (Location(10, -10), 1.0),
      (Location(-10, -10), 2.0),
      (Location(-10, 10), 3.0),
      (Location(10, 10), 4.0)
    )
    val largeSquare = List(
      (Location(50, -50), 1.0),
      (Location(-50, -50), 2.0),
      (Location(-50, 50), 3.0),
      (Location(50, 50), 4.0)
    )
    val pSmall = predictTemperature(smallSquare, Location(0, 0))
    val pLarge = predictTemperature(largeSquare, Location(0, 0))
    assert(math.abs(pSmall - pLarge) < 1e-6)
    assert(math.abs(pSmall - 2.5) < 1e-6)
    assert(math.abs(predictTemperature(smallSquare, Location(10, -10)) - 1.0) < 1e-6)
    assert(math.abs(predictTemperature(smallSquare, Location(10.001, -10.001)) - 1.0) < 1e-6)
  }

  test("color interpolation") {
    val temperatures = List(70, 60, 50, 40, 32, 30, 20, 12, 5, 0, -10, -15, -20, -27, -35, -45, -50, -55, -60, -70)
    val expected = List(
      Color(255,255,255),
      Color(255,255,255),
      Color(255,164,164),
      Color(255,73,73),
      Color(255,0,0),
      Color(255,26,0),
      Color(255,153,0),
      Color(255,255,0),
      Color(106,255,149),
      Color(0,255,255),
      Color(0,85,255),
      Color(0,0,255),
      Color(106,0,255),
      Color(255,0,255),
      Color(178,0,204),
      Color(81,0,139),
      Color(33,0,107),
      Color(17,0,54),
      Color(0,0,0),
      Color(0,0,0)
    )
    val got = temperatures.map(interpolateColor(colorScale, _))
    for ((expectedColor, gotColor) <- expected zip got) {
      assert(expectedColor == gotColor)
    }
  }

  test("lat lon to image coordinates") {
    val locations = List[Location](
      Location(90, -180),
      Location(90, 180),
      Location(-90, -180),
      Location(-90, 180),
      Location(0, 0)
    )
    val expected = List[(Int, Int)](
      (0,0),
      (360,0),
      (0,180),
      (360,180),
      (180,90)
    )
    val got = locations.map(locationToXY)
    for ((expectedXY, gotXY) <- expected zip got) {
      assert(expectedXY == gotXY)
    }
  }

//  test("visualization end to end") {
//    val avgTemperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv"))
//    println(s"len avg temps ${avgTemperatures.size}")
//    val colorScale = List(
//      (60.0, Color(255, 255, 255)),
//      (32.0, Color(255, 0, 0)),
//      (12.0, Color(255, 255, 0)),
//      (0.0, Color(0, 255, 255)),
//      (-15.0, Color(0, 0, 255)),
//      (-27.0, Color(255, 0, 255)),
//      (-50.0, Color(33, 0, 107)),
//      (-60.0, Color(0, 0, 0))
//    )
//    val locTemperatures = for (x <- 0 until 360; y <- 0 until 180) yield {
//      val loc = Location(90 - y, x - 180)
//      val t = predictTemperature(avgTemperatures, loc)
//      (loc, t)
//    }
//    println("creating image")
//    val img = visualize(locTemperatures, colorScale)
//    img.output("target/lala2.png")
//  }
}
