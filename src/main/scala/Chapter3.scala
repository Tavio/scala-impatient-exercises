import java.awt.datatransfer.{DataFlavor, SystemFlavorMap}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Chapter3 extends App {
  // Write a code snippet that sets a to an array of n random integers between
  // 0 (inclusive) and n (exclusive).
  def question1(n : Int) = {
    {for(i <- 0 until n) yield scala.util.Random.nextInt(n)}.to[Array]
  }

  // Write a loop that swaps adjacent elements of an array of integers. For example,
  // Array(1, 2, 3, 4, 5) becomes Array(2, 1, 4, 3, 5).
  def question2(a : Array[Int]) = {
    for(i <- 0 until a.length - 1 by 2) {
      val temp = a(i)
      a(i) = a(i + 1)
      a(i + 1) = temp
    }
    a
  }

  // Repeat the preceding assignment, but produce a new array with the swapped
  // values. Use for/yield.
  def question3ButBetter(a : Array[Int]) = {
    a.grouped(2).map(_.reverse).flatten
  }

  // Given  an  array  of  integers,  produce  a  new  array  that  contains  all  positive
  // values of the original array, in their original order, followed by all values that
  // are zero or negative, in their original order.
  def question4(a : Array[Int]) = {
    val positives = for(i <- 0 until a.length if a(i) >= 0) yield a(i)
    val negatives = for(i <- 0 until a.length if a(i) < 0) yield a(i)
    Array(positives, negatives).flatten
  }

  def question4Alt(a : Array[Int]) = {
    val partitions = a.partition(_ >= 0)
    Array(partitions._1, partitions._2).flatten
  }

  // How do you compute the average of an Array[Double]?
  def question5(a : Array[Double]) = {
    if (a.length == 0) 0.0
    else {
      var sum: Double = 0
      for (i <- 0 until a.length) sum += a(i)
      sum / a.length
    }
  }

  def question5Alt(a : Array[Double]) = {
    if (a.length == 0) 0.0
    else a.reduce(_+_) / a.length
  }

  // How do you rearrange the elements of an Array[Int] so that they appear in
  // reverse sorted order? How do you do the same with an ArrayBuffer[Int]?
  def question6Array(a : Array[Int]) = {
    a.sortWith(_ > _)
  }

  def question6ArrayBuffer(a : ArrayBuffer[Int]) = {
    // Same thing, unless the question is about changing the array in place,
    // which I won't bother with because that's rarely useful.
    a.sortWith(_ > _)
  }

  // Write a code snippet that produces all values from an array with duplicates
  // removed.
  def question7(a : Array[Int]) = {
    a.distinct
  }

  // Rewrite the  example  at  the  end  of Section 3.4, “Transforming Arrays,”
  // on page 34 using the drop method for dropping the index of the first match.
  def question8(a : Array[Int]) = {
    a.dropRight(a.indexWhere(_ < 0) + 1)
  }

  // Make a collection of all time zones returned by java.util.TimeZone.getAvailableIDs
  // that are in America. Strip off the "America/" prefix and sort the result.
  def question9 = {
    java.util.TimeZone.getAvailableIDs.
      filter(_.startsWith("America")).
      map(_.replace("America/", "")).
      sorted
  }

  // Import java.awt.datatransfer._ and make an object of type SystemFlavorMap with the call
  // val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
  // Then call the getNativesForFlavor method with parameter DataFlavor.imageFlavor
  // and get the return value as a Scala buffer. (Why this obscure class? It’s hard
  // to find uses of java.util.List in the standard Java library.)
  def question10 = {
    val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
    val natives : mutable.Buffer[String] = flavors.getNativesForFlavor(DataFlavor.imageFlavor).asScala
    natives
  }

  println(question10)
}

