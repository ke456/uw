package UW_API

import java.util.Calendar

object TermGenerator {
  val termNumbers = Map("fall" -> 9, "winter" -> 1, "spring" -> 5)

  def getCurrentTerm = {
    val date = Calendar.getInstance()
    val y = date.get(Calendar.YEAR)
    val century = y / 100 - 20
    val year = y % 1000

  }

  def generateTerm(year : Int, season : String) ={

  }
}
