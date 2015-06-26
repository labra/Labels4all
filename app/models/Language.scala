package models

import anorm._
import anorm.SqlParser._

import play.api.db.DB
import play.api.Play.current

import java.sql.Connection

case class Language(code: String,
	name: String, 
	id: Long)

object Language {
	
	private val languageParser: RowParser[Language] = {
		get[String]("code") ~
		get[String]("name") ~
		get[Long]("id") map {
			case code ~ name ~ id => Language(code, name, id)
		}
	}
    
    def all(page: Int): Seq[Language] = {
        DB.withConnection { implicit conn =>
            findAll(if(page < 1) 1 
                    else page)
        }
    }
    
    def numberOfLabels(langCode: String): (Option[Language], Int) = {
        DB.withConnection { implicit conn =>
            findByCode(langCode) match {
                case Some(language) => (Some(language), LabelTranslation.numberOfLabels(language.id))
                case None => (None, 0)
            }
        }
    }
	
	private[models] def create(code : String, name : String)(implicit conn: Connection): Option[Long] = {		
		SQL("INSERT INTO Languages(code, name) VALUES({code}, {name})")
			.on('code -> code,
				'name -> name)
			.executeInsert()
	}
	
	private[models] def findById(id: Long)(implicit conn: Connection): Option[Language] = {		
		SQL("SELECT code, name, id FROM Languages WHERE id = {id}")
			.on('id -> id)
			.as(languageParser.singleOpt)
	}
	
	private[models] def findByCode(code: String)(implicit conn: Connection): Option[Language] = {
		SQL("SELECT code, name, id FROM Languages WHERE code = {code}")
			.on('code -> code)
			.as(languageParser.singleOpt)
	}
    
	private[models] def findAll(page: Int)(implicit conn: Connection): Seq[Language] = {
        val limit = 30
        val offset = (page-1) * limit
		SQL("SELECT code, name, id FROM Languages OFFSET {offset} LIMIT {limit}")
			.on('offset -> offset,
                'limit -> limit)
			.as(languageParser.*)
	}
}