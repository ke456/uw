package UW_API
import org.json._
import scala.io.Source

object JSONTool {
  val uw_key = ".json?key=9610e70221a4ce59210fbceb80226224"
  
  @throws(classOf[NoDataFoundException])
  def read(url: String) : JSONObject = {
    val result = new JSONObject (Source.fromURL(url).mkString)
    if (result.getJSONObject("meta").getString("message") == "No data returned") throw new NoDataFoundException()
    else result
  }
}