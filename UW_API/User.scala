package UW_API

class User (userName : String){
  private var name = userName
  private var events = Vector[Event]()
  private var classes = Vector[CourseFinder.Class]()
  private var datedEvents = Vector[DatedEvent]()
  private var classLookout = Vector[CourseFinder.Class]()
  
  private var archivedEvents = Vector[Event]()
  def getArchivedEvents = archivedEvents

  def getName = name
  def setName(newName : String) = name = newName

  def getEvents = events
  def getClasses = classes
  def getDatedEvents = datedEvents
  def getClassLookout = {
    val _ = updateLookout()
    updateLookout()
  }

  implicit def collect (l : Vector[CourseFinder.TestSection]) : Vector[DatedEvent] =
    l.foldRight(Vector[DatedEvent]())((x,rrr) => x.tests ++ rrr)
  implicit def convert (t : CourseFinder.TestSection) : Vector[DatedEvent] =
    t.tests

  def addEvent(e : Event): Unit = e match {
  case e : CourseFinder.Class => classes = classes ++ Vector(e)
  case e : DatedEvent => datedEvents = {
    def insert (l : Vector[DatedEvent], e : DatedEvent) : Vector[DatedEvent] =
      if (l == Vector()) Vector(e)
      else if (e < l.head) e +: l
      else l.head +: insert(l.tail, e)
    insert(datedEvents, e)
  }
  case _ => events = events ++ Vector(e)
  }
  
  def addTestSection (e : CourseFinder.TestSection): Unit = addEvent(e)
  
  def addEvent (e: Vector[Event]): Unit = e.map(addEvent(_))
  def addTestSection (e : Vector[CourseFinder.TestSection]): Unit = collect(e).map(addEvent(_))
  def addToLookout (c : CourseFinder.Class):Unit = classLookout = classLookout ++ Vector(c)
  def addToLookout (l : Vector[CourseFinder.Class]):Unit = l.foreach(addToLookout(_))
  
  def updateLookout() = classLookout.map(x => CourseFinder.getClassSchedule(x("class_number")))
  
  def getAllEvents = events ++ classes ++ datedEvents
  
  def getTests = datedEvents.filter { x => x.contains("TST") }
  
  private def removeFromList[A](e : A, v : Vector[A]): Vector[A] = v.filter { x => x != e } 
  
  def removeFromLookout (e : CourseFinder.Class) = classLookout = removeFromList(e, classLookout)
  def remove (e: Event) = e match {
    case e : CourseFinder.Class => classes = removeFromList(e, classes)
    case e : DatedEvent => datedEvents = removeFromList(e, datedEvents)
    case _ => events = removeFromList(e, events)
  }
  
  private def archive [A <: Event](e : A, l : Vector[A]){
    archivedEvents = archivedEvents ++ Vector(e)
    remove(e)
  }
  
  def moveToArchive[A <: Event](e : A) = e match {
    case e : CourseFinder.Class => archive(e, classes)
    case e : DatedEvent => archive(e, datedEvents)
    case _ => archive(e, events)
  }
}