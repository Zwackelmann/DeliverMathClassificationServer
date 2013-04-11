package models
case class Paper(val title: String, val abstractText: String)
case class TrueClasses(val trueClasses: List[String], val mainClass: String, val paperId: String)