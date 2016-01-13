package UW_API

import org.json._

object CourseFinder {
  private val PREFIX = "https://api.uwaterloo.ca/v2/courses/"

  implicit def convert(value: Vector[Any]): Vector[String] = value.map(convert(_))
  implicit def convert(value: Any): String = if (value == null) "" else value.toString
  implicit val convert = (x: JSONArray) => (0 to (x.length - 1)).map { a => x getJSONObject a }.toVector

  def extractString(a: JSONArray) =
  if (a.length() != 0)
    (0 to (a.length() - 1)).map { x => a.getString(x) }.foldRight("")((x, rrr) => x ++ "\n" ++ rrr).init
  else ""

  val searchTermsSimple = Vector("subject", "catalog_number", "units", "title", "description")
  val resultSimple = (r: JSONObject) => searchTermsSimple.map(r.get(_).toString)

  val searchTermsDetailed = Vector("terms_offered", "prerequisites", "antirequisites", "instructions")
  val resultDetailed =
    (r: JSONObject) =>
      extractString(r.getJSONArray("terms_offered")) +: r.get("prerequisites") +:
        r.get("antirequisites") +: extractString(r.getJSONArray("instructions")) +: Vector()

  val classSchedule = Vector("class_number", "section", "campus", "related_component_1", "related_component_2",
    "enrollment_capacity", "enrollment_total", "start_time", "end_time", "weekdays", "location", "instructors", "terms")

  /**
   * Required fields: subject, catalog_number
   */
  class Course(
      f: Vector[String],
      i: Vector[String]) extends Event(f, i) {
    assert(this.fields.contains("subject") && this.fields.contains("catalog_number"))
  }

  /**
   * Required fields: <code> CourseFinder.searchTermsDetailed </code>
   */
  class CourseDetailed(
      f: Vector[String],
      i: Vector[String]) extends Course(f, i) {
    assert(fields.containsSlice(searchTermsDetailed))
  }

  /**
   * Required fields: <code>CourseFinder.classSchedule</code>
   */
  class Class(
      f: Vector[String],
      i: Vector[String]) extends Course(f, i) {
    assert(fields.containsSlice(classSchedule))

    /**
     * Returns true if the class has space
     */
    def hasSpace = apply("enrollment_total") > apply("enrollment_capacity")
  }

  /**
   * This class provides information for a test section. <br>
   * The course information is stored as fields, while the <br>
   * specific tests are stored in <code>tests</code>
   */
  class TestSection(
      val subject: String,
      val number: String,
      val section: String,
      val notes: String,
      val tests: Vector[DatedEvent]) {
    def apply(i: Int) = tests(i)
  }

  /**
   * Searches through all course listings by subject
   *
   * @param subject the subject to be searched
   * @return all courses that matches subject
   */
  def findCourses(subject: String) = {
    val queryResult = JSONTool.read(PREFIX + subject + JSONTool.uw_key)
    if (queryResult == null) Vector.empty
    else {
      val data = queryResult.getJSONArray("data")
      convert(data).map(x => new Course(
        searchTermsSimple,
        resultSimple(x)))
    }
  }

  private val find = { (x: String) => findCourses(x).filter(_) }

  /**
   * Searches through course listing containing <code>matcher</code>
   * in the course fields
   * 
   * @param subject The subject to be searched
   * @param matcher Field to search for
   * @return Courses that match subject and contains <code>matcher</code>
   * @see CourseFinder.findCourses
   */
  def findCoursesByMatch(subject: String, matcher: String) =
    find(subject)(x => x.contains(matcher))

  /**
   * Searches through course listing containing <code>num</code>
   * in the catalog number
   * 
   * @param subject The subject to be searched
   * @param num Partial (or full) catalog number
   * @return Courses that match subject and contains <code>num</code>
   * @see CourseFinder.findCourses
   */
  def findCoursesByNum(subject: String, num: String) =
    find(subject)(x => x("catalog_number").contains(num))

    /**
     * Search the specific course by <code>subject</code> and  <code>num</code>
     * and returns the full detail of the course
     * 
     * @param subject The subject to be searched
     * @param num Full catalog number
     * @return Full course details of the specified course
     */
  def getDetails(subject: String, num: String): CourseDetailed = {
    val queryResult = JSONTool.read(PREFIX + subject + "/" + num + JSONTool.uw_key).getJSONObject("data")
    new CourseDetailed(
      searchTermsSimple ++ searchTermsDetailed,
      resultSimple(queryResult) ++
        resultDetailed(queryResult))
  }

  /**
   * Convenience function for <code>getDetails(subject, num)</code>
   * 
   * @see CourseFinder.getDetails(subject, num)
   */
  def getDetails(e: Course): CourseDetailed =
    getDetails(e("subject"), e("catalog_number"))

  private val extractClassInfo = (r: JSONArray) => {
    val data = r.getJSONObject(0)
    val date = data.getJSONObject("date")
    val location = data.getJSONObject("location")
    val instructors = data.getJSONArray("instructors")
    val d = Vector(date.get("start_time"), date.get("end_time"), date.get("weekdays"))
    val l = convert(location.get("building")) + " " + convert(location.get("room"))
    val i = (0 to (instructors.length - 1)).map { instructors.getString(_) }.foldRight("") { (x, rrr) => x + "\n" + rrr }
    if (i == "") d ++ Vector(l) ++ Vector("NA")
    else d ++ Vector(l) ++ Vector(i.init)
  }

  private def createClassSchedule(r: JSONObject): Class = {
    val s = Vector("subject", "catalog_number", "units", "title", "note")
    new Class(
      s ++ classSchedule,
      s.map { r.get(_) } ++
        (0 to 6).map(x => r.get(classSchedule(x))) ++
        extractClassInfo(r.getJSONArray("classes")) ++
        Vector(r.get("term")))
  }

  def getCourseSchedule(subject: String, num: String): Vector[Class] = {
    val result = JSONTool.read(PREFIX + subject + "/" + num + "/schedule" + JSONTool.uw_key).getJSONArray("data")
    convert(result).filter { x => !(x.getString("section").startsWith("TST")) }.map(createClassSchedule(_))
  }

  def getCourseSchedule(e: Course): Vector[Class] =
    getCourseSchedule(e("subject"), e("catalog_number"))

  def getClassSchedule(classNum: String) = {
    val r = JSONTool.read(PREFIX + classNum + "/schedule." + JSONTool.uw_key).getJSONArray("data").getJSONObject(0)
    createClassSchedule(r)
  }

  private def createTestSchedule(r: JSONObject) = {
    val subject = r.getString("subject")
    val num = r.getString("catalog_number")
    new TestSection(
      subject,
      num,
      r.get("section"),
      r.get("note"),
      r.getJSONArray("classes").map(x => {
        val d = x.getJSONObject("date")
        val l = x.getJSONObject("location")
        new DatedEvent(Vector(subject + " " + num + " TST") ++ DatedEvent.fields.tail.init.init.map(x => d.get(x)) ++
          Vector(convert(l.get("building")) ++ " " ++ convert(l.get("room"))) ++ Vector("Test"))
      }))
  }

  def getTestSchedule(subject: String, num: String) : Vector[TestSection] = {
    val result = JSONTool.read(PREFIX + subject + "/" + num + "/schedule" + JSONTool.uw_key).getJSONArray("data")
    result.filter { x => x.getString("section").startsWith("TST") }.map(createTestSchedule(_))
  }

  def getTestSchedule (c : Course) : Vector[TestSection] =
    getTestSchedule(c("subject"),c("catalog_number"))
}