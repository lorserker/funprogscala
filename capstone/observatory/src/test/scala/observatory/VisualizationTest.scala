package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import observatory.Visualization._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

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
    val temperatures = List(
      (Location(0, 0), 80.0),
      (Location(0, 45), 10.0),
      (Location(0, 90), 90.0)
    )

    println(predictTemperature(temperatures, Location(10, 45)))
    println(predictTemperature(temperatures, Location(0, 0)))
    println(predictTemperature(temperatures, Location(0.001, 0.001)))
  }

  test("color interpolation") {
    val points = List(
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0))
    )

    println((70, interpolateColor(points, 70)))
    println((60, interpolateColor(points, 60)))
    println((50, interpolateColor(points, 50)))
    println((40, interpolateColor(points, 40)))
    println((32, interpolateColor(points, 32)))
    println((30, interpolateColor(points, 30)))
    println((20, interpolateColor(points, 20)))
    println((12, interpolateColor(points, 12)))
    println((5, interpolateColor(points, 5)))
    println((0, interpolateColor(points, 0)))
    println((-10, interpolateColor(points, -10)))
    println((-15, interpolateColor(points, -15)))
    println((-20, interpolateColor(points, -20)))
    println((-27, interpolateColor(points, -27)))
    println((-35, interpolateColor(points, -35)))
    println((-45, interpolateColor(points, -45)))
    println((-50, interpolateColor(points, -50)))
    println((-55, interpolateColor(points, -55)))
    println((-60, interpolateColor(points, -60)))
    println((-70, interpolateColor(points, -70)))
  }

  test("lat lon to image coordinates") {
    val latLon = List[(Double, Double)](
      (90, -180),
      (90, 180),
      (-90, -180),
      (-90, 180),
      (0, 0)
    )
    for ((lat, lon) <- latLon) {
      println(locationToXY(Location(lat, lon)))
    }
  }

  test("visualization end to end") {
    val avgTemperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv"))
    println(s"len avg temps ${avgTemperatures.size}")
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
    val locTemperatures = for (x <- 0 until 360; y <- 0 until 180) yield {
      val loc = Location(90 - y, x - 180)
      val t = predictTemperature(avgTemperatures, loc)
      (loc, t)
    }
    println("creating image")
    val img = visualize(locTemperatures, colorScale)
    img.output("target/lala.png")
  }
}
