package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val p2z = pow(2, zoom)
    val lon = (x / p2z) * 360 - 180
    val lat = atan(sinh(Pi - 2*Pi*y/p2z)) * 180 / Pi
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val topLeft = tileLocation(zoom, x, y)
    val bottomRight = tileLocation(zoom, x+1, y+1)
    val pixels: Array[Pixel] = for (i <- (0 until 256*256).toArray) yield Pixel(0, 0, 0, 127)
    val z2 = 128 * pow(2, zoom)
    for(row <- 0 until 256; col <- 0 until 256) {
      val lat = (180/Pi) * (2*atan(exp((Pi*(z2 - 256*y - row))/z2)) - Pi/2)
      val lon = 180 * (x * 256 + col - z2) / z2
      val location = Location(lat, lon)
      val temperature = Visualization.predictTemperature(temperatures, location)
      val color = Visualization.interpolateColor(colors, temperature)
      val i = row * 256 + col
      pixels(i) = Pixel(color.red, color.green, color.blue, 127)
    }
    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    ???
  }

}
