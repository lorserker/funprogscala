package observatory

import math._
import com.sksamuel.scrimage.{Image, Pixel}

import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val distTemp = temperatures.map { case (loc, t) => (circleDistance(loc, location), t) }
    val p = 3.0
    var wSum = 0.0
    var s = 0.0
    var nNear = 0
    var sNear = 0.0
    for ((dist, temp) <- distTemp) {
      if (dist < 1) {
        nNear += 1
        sNear += temp
      } else {
        val w = 1 / pow(dist, p)
        wSum = wSum + w
        s = s + w * temp
      }
    }
    if (nNear > 0) sNear / nNear else s / wSum
  }

  def circleDistance(loc1: Location, loc2: Location): Double = {
    def radians(degrees: Double) = Pi * degrees / 180

    val deltaLat = radians(abs(loc1.lat - loc2.lat))
    val deltaLon = radians(abs(loc1.lon - loc2.lon))

    val centralAngle = 2 * asin(sqrt(pow(sin(deltaLat/2), 2) + cos(radians(loc1.lat)) * cos(radians(loc2.lat)) * pow(sin(deltaLon/2), 2)))

    centralAngle * 6371  // times radius of the earth in km
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val sortedPoints = points.toArray.sortBy(_._1)
    val i = firstLargerIndex(0, value, sortedPoints)
    if (i == 0) {
      sortedPoints.head._2
    } else if (i >= sortedPoints.length) {
      sortedPoints.last._2
    } else {
      val color0 = sortedPoints(i-1)._2
      val color1 = sortedPoints(i)._2
      val t = (value - sortedPoints(i-1)._1) / (sortedPoints(i)._1 - sortedPoints(i-1)._1)

      Color(
        math.round(color0.red + t * (color1.red - color0.red)).toInt,
        math.round(color0.green + t * (color1.green - color0.green)).toInt,
        math.round(color0.blue + t * (color1.blue - color0.blue)).toInt
      )
    }
  }

  @tailrec
  def firstLargerIndex(i: Int, v: Double, points: Array[(Double, Color)]): Int = {
    if (i >= points.length || points(i)._1 > v)
      i
    else
      firstLargerIndex(i+1, v, points)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val pixels = new Array[Pixel](360 * 180)
    for (i <- 0 until pixels.length) {
      pixels(i) = Pixel(0, 0, 0, 255)
    }
    println(pixels.length)
    for ((loc, temp) <- temperatures) yield {
      val color = interpolateColor(colors, temp)
      val (x, y) = locationToXY(loc)
      val i = xyToIndex(x, y, 360, 180)
      pixels(i) = Pixel(color.red, color.green, color.blue, 255)
    }
    Image(360, 180, pixels)
  }

  def locationToXY(loc: Location): (Int, Int) = {
    val xImg = math.round(loc.lon + 180).toInt
    val yImg = math.round(-loc.lat + 90).toInt

    (xImg, yImg)
  }

  def xyToIndex(x: Int, y: Int, width: Int, height: Int): Int = {
    y * width + x
  }

}

