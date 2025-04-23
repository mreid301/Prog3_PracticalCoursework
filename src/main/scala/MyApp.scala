import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap

/**
 * Food Basket Price Analysis Application - Coursework
 * Author: Matthew Reid
 * Version: 1.0
 */

object MyApp extends App {

  // *******************************************************************************************************************
  // application logic

  // read data from file
  val mapdata: Map[String, List[Int]] = readFile("data.txt")
  // print data to check it's been read in correctly
  println(s"Loaded ${mapdata.size} items")

  // define menu options as a Map of actions
  val actionMap = Map[
    Int, () => Boolean
  ](
    1 -> handleCurrentPrices,
    2 -> handleHighLow,
    3 -> handleMedian,
    4 -> handleCompareAverage,
    5 -> handleBasketTotal,
    6 -> handleQuit
  )

  // loop to read input and invoke menu option
  var opt = 0
  while ( {
    opt = readOption
    menu(opt)
  }) ()

  // *******************************************************************************************************************
  // FUNCTIONS FOR MENU

  def readOption: Int = {
    println(
      """|
         |Please select one of the following:
         | 1 - Show current price for each food
         | 2 - Show highest and lowest prices per food
         | 3 - Show median price per food
         | 4 - Compare average price of two foods
         | 5 - Calculate total basket price
         | 6 - Quit
         |Select>""".stripMargin)
    readInt()
  }

  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None => println("Invalid selection"); true
    }
  }

  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  def readFile(filename: String): Map[String, List[Int]] = {
    try {
      Source.fromFile(filename).getLines().flatMap { line =>
        val parts = line.split(",").map(_.trim)
        if (parts.length == 25) {
          val food = parts.head
          val prices = parts.tail.map(_.toInt).toList
          Some(food -> prices)
        } else {
          println(s"Warning: Skipped malformed line: $line"); None
        }
      }.toMap
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Error: data.txt file not found."); Map()
      case _: NumberFormatException =>
        println("Error: Invalid number format in data file."); Map()
    }
  }

  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER

  def handleCurrentPrices(): Boolean = {
    val result = ListMap(mapdata.view.mapValues(_.last).toSeq.sortWith(_._2 > _._2): _*)
    result.foreach { case (f, p) => println(s"$f: ${p}p") }
    true
  }

  def handleHighLow(): Boolean = {
    mapdata.foreach { case (f, prices) =>
      println(s"$f: High = ${prices.max}p, Low = ${prices.min}p")
    }
    true
  }

  def handleMedian(): Boolean = {
    mapdata.foreach { case (f, prices) =>
      val sorted = prices.sorted
      val median = if (sorted.size % 2 == 0)
        (sorted(sorted.size / 2 - 1) + sorted(sorted.size / 2)) / 2.0
      else sorted(sorted.size / 2).toDouble
      println(f"$f: $median%.1fp")
    }
    true
  }

  def handleCompareAverage(): Boolean = {
    print("Enter first food: "); val f1 = readLine().toUpperCase
    print("Enter second food: "); val f2 = readLine().toUpperCase

    val avg = (l: List[Int]) => l.sum.toDouble / l.size
    (mapdata.get(f1), mapdata.get(f2)) match {
      case (Some(p1), Some(p2)) =>
        println(f"$f1 average: ${avg(p1)}%.2fp")
        println(f"$f2 average: ${avg(p2)}%.2fp")
      case _ => println("One or both food items not found.")
    }
    true
  }

  def handleBasketTotal(): Boolean = {
    println("Enter basket items in the form FOOD:AMOUNT. Leave blank to finish.")
    val basket: Map[String, Float] = LazyList
      .continually(readLine().trim)
      .takeWhile(_.nonEmpty)
      .flatMap { input =>
        input.split(":").map(_.trim) match {
          case Array(food, amt) if amt.toFloatOption.isDefined =>
            val upper = food.toUpperCase
            if (mapdata.contains(upper)) Some(upper -> amt.toFloat)
            else { println(s"$upper not recognised."); None }
          case _ => println("Invalid format."); None
        }
      }
      .groupMapReduce(_._1)(_._2)(_ + _)

    val latestPrices = mapdata.view.mapValues(_.last).toMap
    val total = basket.foldLeft(0.0) { case (sum, (item, qty)) => sum + qty * latestPrices(item) }
    println(f"Total basket cost: £${total / 100}%.2f")
    true
  }

  def handleQuit(): Boolean = {
    println("Quitting application.")
    false
  }

  // *******************************************************************************************************************

}