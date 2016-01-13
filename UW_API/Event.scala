package UW_API

import org.json._;

class Event(
    val fields: Vector[String],
    val info: Vector[String]) {
  assert(fields.size == info.size)

  def contains(x: String) =
    info.foldRight(false)((i, r) => i.contains(x) || r)

  def apply(field: String): String = info(fields.indexOf(field))

  def apply(index: Int): String = info(index)

  def getPair = {
    def getPair2 (f : Vector[String], i: Vector[String]) : Vector[(String,String)] = f match {
      case Vector() => Vector()
      case _ => (f.head,i.head) +: getPair2(f.tail,i.tail)
    }
    getPair2(fields,info)
  }
  
  override def toString = info.foldRight("")((x, rrr) => x ++ "\n" ++ rrr).init
  override def equals(that : Any) = that match {
    case that : Event => that.info == this.info
    case _ => false
  }
}

