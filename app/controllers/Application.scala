package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models.Paper
import models.TrueClasses
import java.io.File
import play.api.libs.concurrent.Akka
import play.api.Play.current
import play.api.libs.json.Json
import script.AllClassesForDocument
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import play.api.libs.concurrent.Promise
import java.util.Date
import java.security.MessageDigest
import java.sql.DriverManager
import java.sql.Statement
import scala.collection.mutable

object CookieGenerator {
    def randomStream: Stream[Int] = (math.random * 10).toInt #:: randomStream

    def hash(str: String) = {
        val md = MessageDigest.getInstance("SHA-256")
        md.update(str.getBytes())
        bytesToHex(md.digest())
    }
    
    val hexArray = Array('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F')
    def bytesToHex(bytes: Array[Byte]) = {
        val hexChars = Array.ofDim[Char](bytes.length * 2)
        
        var v: Int = 0
        for (j <- 0 until bytes.length) {
            v = bytes(j) & 0xFF
            hexChars(j * 2) = hexArray(v >>> 4)
            hexChars(j * 2 + 1) = hexArray(v & 0x0F)
        }
        
        new String(hexChars)
    }


    def apply() = {
        val timestamp = new Date().getTime().toBinaryString
        val randomNumber = randomStream.take(32).toList.mkString
        hash(timestamp + randomNumber)
    }
}

object Application extends Controller {
    val conn = DriverManager.getConnection("jdbc:derby:delivermathdata;create=true")

    try {
        val stmt = conn.createStatement()
        stmt.execute("""CREATE TABLE request(
            user_id CHAR(64),
            paper_id CHAR(64) PRIMARY KEY,
            paper_abstract LONG VARCHAR,
            paper_title LONG VARCHAR
        )""")

        stmt.execute("""CREATE TABLE suggested_classes(
            paper_id CHAR(64),
            msc_code CHAR(5),
            certainty FLOAT
        )""")

        stmt.execute("""CREATE TABLE true_classes(
            paper_id CHAR(64),
            true_class CHAR(5),
            main_class CHAR(1)
        )""")
    } catch {
        case ex: Throwable => // ex.printStackTrace
    }

    val insertRequestStmt = conn.prepareStatement("INSERT INTO request (user_id, paper_id, paper_abstract, paper_title) VALUES (?, ?, ?, ?)")
    val insertSuggestionStmt = conn.prepareStatement("INSERT INTO suggested_classes (paper_id, msc_code, certainty) VALUES (?, ?, ?)")
    val insertTrueStmt = conn.prepareStatement("INSERT INTO true_classes (paper_id, true_class, main_class) VALUES (?, ?, ?)")

    val paperForm = Form(
        mapping(
            "title_text" -> text,
            "abstract_text" -> text
        )(Paper.apply)(Paper.unapply)
    )

    val trueClassesForm = Form(
        mapping(
            "true_classes" -> list(text),
            "main_class" -> text,
            "paper_id" -> text
        )(TrueClasses.apply)(TrueClasses.unapply)
    )

    val queryEvaluator = AllClassesForDocument()

    private def findCategories(abstractText: String, titleText: String): List[Pair[String, Double]] = {
        val queryList = queryEvaluator.findCategories(abstractText, titleText)
        if(queryList.forAll(_._1.length() == 5)) {
            val topLevelClassif = queryList.filter(cl => cl._1.substring(2, 5) == "___").sortWith((a, b) => a._2 > b._2)
            val secondLevelClassif = queryList.filter(cl => cl._1.charAt(2) != "_" && cl._1.substring(3, 5) == "__").sortWith((a, b) => a._2 > b._2)
            val thirdLevelClassif = queryList.filter(cl => cl._1.charAt(4) != "_").sortWith((a, b) => a._2 > b._2)

            val structuredQueryList = new mutable.ListBuffer[(String, Double)]
            for(topLevelClassification <- topLevelClassif) {
                structuredQueryList += topLevelClassification
                val fittingSecondLevelClassif = secondLevelClassif.filter(cl => cl._1.substring(0,2) == topLevelClassification._1.substring(0,2))
                for(secondLevelClassification <- fittingSecondLevelClassif) {
                    structuredQueryList += secondLevelClassification
                    val fittingThirdLevelClassif = thirdLevelClassif.filter(cl => cl._1.substring(0,3) == secondLevelClassification._1.substring(0,3))
                    structuredQueryList ++= fittingThirdLevelClassif
                }   
            }
            structuredQueryList.toList
        } else {
            queryList
        }

        // List(("01A05", 0.90), ("05C35", 0.85), ("05A__", 0.95), ("90K55", 0.55), ("80B__", 0.6), ("60C30", 0.75))
    }

    def index = Action { implicit request => 
        val cookie = request.cookies.get("delivermath") match {
            case Some(cookie) => Cookie(cookie.name, cookie.value, Some(60*60*24*100))
            case None => Cookie("delivermath", CookieGenerator(), Some(60*60*24*100))
        }

        Ok(views.html.classify(paperForm)).withCookies(cookie)
    }

    def trueClasses = Action { implicit request => 
        trueClassesForm.bindFromRequest.fold(
            formWithErrors => Ok(Json.toJson(Map("success" -> "false", "msg" -> "invalid form parameters"))),
            trueClasses => {
                for(c <- trueClasses.trueClasses) {
                    insertTrueStmt.setString(1, trueClasses.paperId)
                    insertTrueStmt.setString(2, c)
                    insertTrueStmt.setString(3, (if(c == trueClasses.mainClass) "1" else "0"))
                    insertTrueStmt.executeUpdate
                }
                Ok(Json.toJson(Map("success" -> Json.toJson("true"))))
            }
        )
    }

    def result = Action { implicit request =>
        paperForm.bindFromRequest.fold(
            formWithErrors => Ok(Json.toJson(Map("success" -> "false", "msg" -> "invalid form parameters"))),
            paper => {
                val futureClasses = Future { findCategories(paper.abstractText, paper.title) }
                val timeoutFuture = Promise.timeout("Calculation takes to long", 60000)

                Async {
                    Future.firstCompletedOf(Seq(futureClasses, timeoutFuture)).map {
                        case cl: List[Pair[String, Double]] => {
                            val generatedPaperId = CookieGenerator()

                            val value = request.cookies.get("delivermath").get.value
                            insertRequestStmt.setString(1, value)
                            insertRequestStmt.setString(2, generatedPaperId)
                            insertRequestStmt.setString(3, paper.abstractText)
                            insertRequestStmt.setString(4, paper.title)
                            insertRequestStmt.executeUpdate()
                            

                            for(c <- cl) {
                                insertSuggestionStmt.setString(1, generatedPaperId)
                                insertSuggestionStmt.setString(2, c._1)
                                insertSuggestionStmt.setDouble(3, c._2)
                                insertSuggestionStmt.executeUpdate()
                            }

                            val labelsSorted = cl.sortWith((a, b) => a._2 > b._2)
                            Ok(Json.toJson(Map(
                                "success" -> Json.toJson("true"),
                                "paper_id" -> Json.toJson(generatedPaperId),
                                "classes" -> Json.toJson("[" + labelsSorted.map(c => "[\"" + c._1.substring(c._1.length-5, c._1.length) + "\"," + c._2 + "]").mkString(",") + "]")
                            )))
                        }
                        case t: String => InternalServerError(t)
                    }
                }
            }
        )
    }
}