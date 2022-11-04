package ru.victorytech
import Numeric.Implicits._
import scala.util.control.Breaks.{break, breakable}

object Main {

  var GREETING = "Hello, Scala!";

  def main(args: Array[String]) = {
    println(GREETING);
    revString(GREETING);
    toLower(GREETING);
    delSym(GREETING, "!");
    appendString(GREETING, "and goodbye python!");
    printClearMonthlyIncome(1000000.00, 10, 3000);
    sortIncome(List(1000, 111, 23, 123123));
    insertNewIncome(600);
    printMiddle(500, 800);
    indexIncome();
  }

  def revString(str: String): Unit = {
    println(str.reverse);
  }

  def toLower(str: String): Unit = {
    println(str.toLowerCase());
  }

  def delSym(str: String, sym: String): Unit = {
    println(str.replace(sym, ""));
  }

  def appendString(str: String, append: String): Unit = {
    println(str + " " + append);
  }

  def printClearMonthlyIncome(yearAmount: Double, bonusPercent: Integer, compensation: Double): Unit = {
    val monthlyBonus = yearAmount / 100 * bonusPercent / 12;
    val monthlyCompensation = compensation / 12;
    val clearMonthlyAmount = (yearAmount - (yearAmount / 100 * 13)) / 12;
    println("Monthly Income: " + String.valueOf(monthlyBonus + monthlyCompensation + clearMonthlyAmount));
  }

  def sortIncome[T: Numeric](values: List[T]): Unit = {
    values.sorted foreach println;
  }

  def insertNewIncome(newIncome: Integer): Unit = {
    println("-----------");
    val initList = List(100, 200, 300, 400, 500);
    breakable { for (i <- 1 to initList.size) {
      if (initList.apply(i - 1) > newIncome) {
        val newList = insert(initList, i - 1, newIncome);
        newList foreach println;
        break;
      } else if (i == initList.size) {
        initList :+ newIncome foreach println;
      }
    }}
  }

  def insert(list: List[Any], i: Int, value: Any): List[Any] = list match {
    case head :: tail if i > 0 => head :: insert(tail, i-1, value)
    case _ => value :: list
  }

  def printMiddle(min: Integer, max: Integer): Unit = {
    println("--------------");
    val initList = List(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100);
    for (i <- 1 to initList.size) {
      if (initList.apply(i - 1) > min && initList.apply(i - 1) < max)
        println(i);
    }
  }

  def indexIncome(): Unit = {
    val initList = List(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100);
    for (i <- 1 to initList.size) {
      println((initList.apply(i - 1) / 100 * 7) + initList.apply(i - 1));
    }
  }
}