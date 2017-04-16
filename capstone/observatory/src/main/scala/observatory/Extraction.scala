package observatory

import java.io.InputStream
import java.time.LocalDate
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = loadStations(getClass.getResourceAsStream(stationsFile))

    Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile))
      .getLines
      .map(_.trim.split(","))
      .flatMap(cols => {
        Try {
          val stationKey = (cols(0), cols(1))
          val location = stations(stationKey)
          val date = LocalDate.of(year, cols(2).toInt, cols(3).toInt)
          val temperature = cols(4).toDouble
          if (temperature > 9999.0) throw new Exception("missing temperature")
          (date, location, fahrenheitToCelsius(temperature))
        }.toOption
      }).toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val locationStats = mutable.Map.empty[Location, (Double, Double)]

    for (record <- records) {
      val location = record._2
      val temperature = record._3
      val s = locationStats.getOrElse(location, (0.0, 0.0))
      locationStats(location) = (s._1 + temperature, s._2 + 1.0)
    }

    locationStats.map{ case (loc, (s, n)) => loc -> s / n }
  }

  def loadStations(is: InputStream): Map[(String, String), Location] = {
    val buffStream = Source.fromInputStream(is)
    buffStream
      .getLines
      .map(_.trim.split(","))
      .filter(_.length == 4)
      .map(cols => (cols(0), cols(1)) -> Location(cols(2).toDouble, cols(3).toDouble))
      .toMap
  }

  def fahrenheitToCelsius(fahrenheit: Double): Double = (fahrenheit - 32) * 5 / 9

}
