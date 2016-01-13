package UW_API

import org.json._
import UW_API.CourseFinder.convert

object EventFinder {
  val prefix = "https://api.uwaterloo.ca/v2/events"

  class UWEvent
  (f : Vector[String], i : Vector[String]) extends Event(f,i) with Ordered[UWEvent] {
    assert(f.containsSlice(eventFields))

    override def compare(that: UWEvent): Int = that("id") compareTo this("id")
  }

  class DatedUWEvent
  (i : Vector[String]) extends DatedEvent(i){

  }

  def eventFields = Vector("id","site_name","title","link","type", "start","end")

  def getAllEvents = {
    def createSection (ob : JSONObject) : Vector[UWEvent] = {
      val info = Vector("id","site_name","title","link").map(ob.get(_)) ++
        Vector(CourseFinder.extractString(ob.getJSONArray("type")))
      convert(ob.getJSONArray("times")) map {x =>
        new UWEvent (
        eventFields,
        info ++ Vector(x.get("start"),x.get("end"))
        )
      }
    }
    def createEvent(arr : Vector[JSONObject]) : Vector[UWEvent] = arr match {
      case Vector() => Vector()
      case _ => createSection(arr.head) ++ createEvent(arr.tail)
    }
    val arr = JSONTool.read(prefix + JSONTool.uw_key).getJSONArray("data")
    createEvent(arr)
  }
}
