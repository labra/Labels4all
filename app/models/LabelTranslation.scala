package models

import anorm._
import anorm.SqlParser._

import play.api.db.DB
import play.api.Play.current

import java.sql.Connection

case class LabelTranslation(language: Language, 
	content: String,
	machineTranslated: Boolean,
	votes: Int,
	id: Long)

object LabelTranslation {
    
	private val labelTranslationParser: RowParser[LabelTranslation] = {
		getAliased[Long]("lid") ~
		get[String]("code") ~
		get[String]("name") ~
		get[String]("content") ~
		getAliased[Boolean]("machineTranslated") ~
		get[Int]("votes") ~
		getAliased[Long]("tid") map {
			case lid ~ code ~ name ~ content ~ 
				 machineTranslated ~ votes ~ 
                 tid => LabelTranslation(Language(code, name, lid), content, machineTranslated, votes, tid)
		}
	}
    
    private[models] def numberOfLabels(langId: Long)(implicit conn: Connection): Int = {
        SQL("""
            SELECT COUNT(*) FROM LabelTranslations WHERE id_language = {langId}
        """)
        .on('langId -> langId)
        .as(scalar[Int].single)
    }
    
    private[models] def findMostVotedLabelEveryLanguage(iriId: Long)
                (implicit conn: Connection): Seq[LabelTranslation] = {
        SQL("""
            SELECT DISTINCT ON (id_language)
                l.id as lid, code, name, content,
                machine_translated as machineTranslated, votes, t.id as tid
            FROM Languages l, LabelTranslations t
            WHERE t.id_iri = {iriId} AND t.id_language = l.id
            ORDER BY t.id_language, votes DESC
        """)
        .on('iriId -> iriId)
        .as(labelTranslationParser.*)
    }
    
    private[models] def findMostVotedLabelSelectedLanguages(iriId: Long, langCodes: Seq[String])
                (implicit conn: Connection): Seq[LabelTranslation] = {
        SQL("""
            SELECT DISTINCT ON (id_language)
                l.id as lid, code, name, content,
                machine_translated as machineTranslated, votes, t.id as tid
            FROM Languages l, LabelTranslations t
            WHERE t.id_iri = {iriId} AND t.id_language = l.id AND code IN ({langCodes})
            ORDER BY t.id_language, votes DESC
        """)
        .on('iriId -> iriId,
            'langCodes -> langCodes)
        .as(labelTranslationParser.*)
    }
    
    private[models] def findAllLabelsSelectedLanguages(iriId: Long, langCodes: Seq[String])
                (implicit conn: Connection): Seq[LabelTranslation] = {
        SQL("""
            SELECT
                l.id as lid, code, name, content,
                machine_translated as machineTranslated, votes, t.id as tid
            FROM Languages l, LabelTranslations t
            WHERE t.id_iri = {iriId} AND t.id_language = l.id AND code IN ({langCodes})
        """)
        .on('iriId -> iriId,
            'langCodes -> langCodes)
        .as(labelTranslationParser.*)
    }
    
    private[models] def addNewLabels(iriId: Long, labels: Map[String, Seq[String]], machineTranslated: Boolean = false)
                (implicit conn: Connection): Seq[Long] = {
        
        val langCodes = labels.keys.toSeq
        val existingLabels = findAllLabelsSelectedLanguages(iriId, langCodes).groupBy(_.language.code)
        
        var toBeInserted = Seq.empty[(Long, Seq[String])]
        for((k, v) <- labels) {
            val l = existingLabels.getOrElse(k, Seq.empty[LabelTranslation])
            val lang = if(l.isEmpty) Language.findByCode(k) else Some(l(0).language)
            
            lang match {
                case Some(lang) => {
                    val newLabels = v.filterNot(n => l.exists(e => e.content.equalsIgnoreCase(n.trim)))
                    if(!newLabels.isEmpty) toBeInserted = toBeInserted :+ (lang.id, newLabels)
                }
                case None => // TODO: Handle case
            }
        }

        if(toBeInserted.isEmpty) {
            Seq.empty
        } else {
            val indexedToBeInserted = toBeInserted.zipWithIndex
            
            val rows = indexedToBeInserted.map { case (labelInfo, i) =>
                s"""({id_iri_${i}}, {id_language_${i}}, {content_${i}}, 
                    {machine_translated_${i}}, {votes_${i}})"""
            }.mkString(",")
            
            val parameters = indexedToBeInserted.flatMap { case(labelInfo, i) =>
                Seq(
                    NamedParameter(s"id_iri_${i}", iriId),
                    NamedParameter(s"id_language_${i}", labelInfo._1),
                    NamedParameter(s"content_${i}", labelInfo._2),
                    NamedParameter(s"machine_translated_${i}", machineTranslated),
                    NamedParameter(s"votes_${i}", 0)
                ) 
            }
            
			SQL("""
                INSERT INTO LabelTranslations(id_iri, id_language, content, 
                    machine_translated, votes) VALUES 
                """ + rows)
				.on(parameters : _*)
				.executeInsert(scalar[Long].*)
        }
    }
}




