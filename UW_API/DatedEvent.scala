package UW_API

class DatedEvent(
    i : Vector[String]) extends Event(
        DatedEvent.fields,
        i) with Ordered[DatedEvent]{
  def compare(that : DatedEvent) =
    if (this("start_date") == that("start_date")) this("start_time").compare(that("start_time"))
    else this("start_date").compare(that("start_date"))
}

object DatedEvent {
  val fields = Vector("title","start_date","end_date","start_time","end_time","location","description")
}