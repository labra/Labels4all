package models

import anorm._
import anorm.SqlParser._

import play.api.db.DB
import play.api.Play.current

import models.dto.IRIWithLabels
import ctx.Contexts.jobsDisp

import scala.concurrent.Future
import scala.util.{Success, Failure}

import PartialFunction.condOpt
import java.sql.Connection

case class IRI(name: String,
	labels: Map[Language, Seq[LabelTranslation]],
	id: Long)
	
object IRI {
	
	private val iriParser: RowParser[IRI] = {
		get[String]("name") ~
		get[Long]("id") map {
			case  name ~ id => IRI(name, Map.empty[Language, Seq[LabelTranslation]], id)
		}
	}
    
    def mostVotedLabelEveryLanguage(name: String): Option[IRI] = {
		DB.withConnection { implicit conn =>
            condOpt(findByName(name)) {
                case Some(iri) => {
                    val labels = LabelTranslation.findMostVotedLabelEveryLanguage(iri.id)
                    IRI(iri.name, labels.groupBy(_.language), iri.id)
                }
            }
		}
    }
    
    def mostVotedLabelSelectedLanguages(name: String, langCodes: Seq[String]): Option[IRI] = {
		DB.withConnection { implicit conn =>
            condOpt(findByName(name)) {
                case Some(iri) => {
                    val labels = LabelTranslation.findMostVotedLabelSelectedLanguages(iri.id, langCodes)
                    IRI(iri.name, labels.groupBy(_.language), iri.id)
                }
            }
		}
    }
    
    def allLabelsSelectedLanguage(name: String, langCode: String): Option[IRI] = {
		DB.withConnection { implicit conn =>
            condOpt(findByName(name)) {
                case Some(iri) => {
                    val labels = LabelTranslation.findAllLabelsSelectedLanguages(iri.id, Seq(langCode))
                    IRI(iri.name, labels.groupBy(_.language), iri.id)
                }
            }
		}
    }
    
    def createAndAddLabels(iriWithLabels: IRIWithLabels): Seq[Long] = {
        DB.withTransaction { implicit conn =>
            val iriId = findByName(iriWithLabels.name) match {
                case None => create(iriWithLabels.name).get // TODO: check option id?
                case Some(i) => i.id
            }
            
            LabelTranslation.addNewLabels(iriId, iriWithLabels.labels)
        }
    }
    
    def createManyAndAddLabels(iriWithLabels: Seq[IRIWithLabels]): Option[Long] = {
        val t = Job.create()
        
        condOpt(t) {
            case Some(ticket) => {
                Future {
                    DB.withTransaction { implicit conn =>
                       for(iwl <- iriWithLabels) yield createAndAddLabels(iwl)
                    }                    
                }.onComplete {
                    case Success(result) => {
                        val numIris = result.size
                        val numLabels = result.flatten.size
                        Job.saveResult(ticket, numIris, numLabels)
                    }
                    case Failure(ex) => {
                        Job.nullifyTicket(ticket)
                    }
                }
            
                ticket
            }
        }
    }
	
	private[models] def create(name: String)(implicit conn: Connection): Option[Long] = {
		SQL("INSERT INTO IRIs(name) VALUES({name})")
			.on('name -> name)
			.executeInsert()
	}
	
	private[models] def findById(id: Long)(implicit conn: Connection): Option[IRI] = {
		SQL("SELECT name, id FROM IRIs WHERE id = {id}")
			.on('id -> id)
			.as(iriParser.singleOpt)
	}
	
	private[models] def findByName(name: String)(implicit conn: Connection): Option[IRI] = {		
		SQL("SELECT name, id FROM IRIs WHERE name = {name}")
			.on('name -> name)
			.as(iriParser.singleOpt)
	}
}