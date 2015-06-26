package models

import anorm._
import anorm.SqlParser._

import play.api.db.DB
import play.api.Play.current

import play.api.libs.json.{Json, JsValue, JsObject, JsNull}

import PartialFunction.condOpt
import java.sql.Connection

case class Job(ticket: Long,
	redeemed: Boolean,
    result: Option[JsValue])

object Job {
	
	private val jobParser: RowParser[Job] = {
		get[Long]("ticket") ~
		get[Boolean]("redeemed") ~
		get[Option[JsValue]]("result") map {
			case ticket ~ redeemed ~ result => Job(ticket, redeemed, result)
		}
	}
    
    implicit def rowToJson: Column[JsValue] = Column.nonNull { 
        (value, meta) => 
            val MetaDataItem(qualified, nullable, clazz) = meta
            value match {
                case pgObject: org.postgresql.util.PGobject => Right(Json.parse(pgObject.getValue))
                case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" +
                            value.asInstanceOf[AnyRef].getClass + " to JsValue for column " + qualified))
            }
    }
    
	def create(): Option[Long] = {		
		DB.withConnection { implicit conn =>
            creat()
		}
	}
    
    def saveResult(ticket: Long, numIris: Int, numLabels: Int) = {
        DB.withConnection { implicit conn =>
            updateResult(ticket, numIris, numLabels)
        }
    }
    
    def nullifyTicket(ticket: Long) = {
        DB.withConnection { implicit conn =>
            nullify(ticket)
        }
    }
    
    def redeemTicket(ticket: Long): Option[Job] = {
        DB.withConnection { implicit conn =>
            redeem(ticket)
        }
    }
    
	private[this] def creat()(implicit conn: Connection): Option[Long] = {		
		SQL("INSERT INTO Jobs (redeemed, result) VALUES(FALSE, NULL)")
			.executeInsert()
	}
    
    private[this] def updateResult(ticket: Long, numIris: Int, numLabels: Int)(implicit conn: Connection) = {
        val result = Json.obj("iris" -> numIris, "labels" -> numLabels).toString
        val pgObject = new org.postgresql.util.PGobject()
        
        pgObject.setType("json")
        pgObject.setValue(result)
        
        SQL("UPDATE Jobs SET result = {result} WHERE ticket = {ticket}")
           .on('result -> anorm.Object(pgObject),
               'ticket -> ticket)
           .executeUpdate()
    }
    
    private[this] def nullify(ticket: Long)(implicit conn: Connection) = {
        val pgObject = new org.postgresql.util.PGobject()
        
        pgObject.setType("json")
        pgObject.setValue("null")
        
        SQL("UPDATE Jobs SET result = {result}, redeemed = 't' WHERE ticket = {ticket}")
           .on('result -> anorm.Object(pgObject),
               'ticket -> ticket)
           .executeUpdate()
    }
    
    private[this] def redeem(ticket: Long)(implicit conn: Connection): Option[Job] = {
        condOpt(findByTicket(ticket)) {
            case Some(job) => {
                if(!job.redeemed && job.result.isDefined) {
                    SQL("UPDATE Jobs SET redeemed = 't' WHERE ticket = {ticket}")
                       .on('ticket -> ticket)
                       .executeUpdate()
                }
                
                job
            }
        }
    }
       
	private[models] def findByTicket(ticket: Long)(implicit conn: Connection): Option[Job] = {
		SQL("SELECT ticket, redeemed, result FROM Jobs WHERE ticket = {ticket}")
			.on('ticket -> ticket)
			.as(jobParser.singleOpt)
	}
}