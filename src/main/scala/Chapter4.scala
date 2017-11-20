import java.awt.datatransfer.{DataFlavor, SystemFlavorMap}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Chapter4 extends App {

  // Set up a map of prices for a number of gizmos that you covet. Then produce
  // a second map with the same keys and the prices at a 10 percent discount.
  def question1() = {
    val prices = Map("Bicycle" -> 250.0, "House" -> 800000.0, "Nintendo Switch" -> 550.0)
    prices.map(t => (t._1, t._2 * 0.9))
  }

  // Write a program that reads words from a file. Use a mutable map to count how often each word appears.
  // To read the words, simply use a java.util.Scanner:
  //
  // val in = new java.util.Scanner(java.io.File("myfile.txt"))
  // while (in.hasNext()) process in.next()
  //
  // Or look at Chapter 9 for a Scalaesque way. At the end, print out all words and their counts.
  def question2() = {
    val source = Source.fromFile("nine_billion_names_of_god.txt", "UTF-8")
    val lineIterator = source.getLines
    val wordsMap = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    val wordsRegex = "\\b[\\w]+\\b".r
    for (l <- lineIterator; m <- wordsRegex.findAllMatchIn(l)) wordsMap(m.toString()) += 1
    for((word, count) <- wordsMap) println(word + ": " + count)
  }

  question2()
}

