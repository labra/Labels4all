package models.json

import models._
import play.api.libs.json.Json
import play.api.libs.json.Writes

object Writers {
    implicit val WritesLanguage = Writes[Language] {
        case Language(code, name, id) => {
            Json.obj(
                "code" -> code,
                "name" -> name
            )
        }
    }
    
    implicit val WritesLanguageWithNumLabels = Writes[(Language, Int)] {
        case (Language(code, name, id), nlabels) => {
            Json.obj(
                "code" -> code,
                "name" -> name,
                "nlabels" -> nlabels
            )
        }
    }
    
    implicit val WritesLabelTranslation = Writes[LabelTranslation] {
        case LabelTranslation(language, content, machineTranslated, votes, id) => {
            Json.obj(
                "content" -> content,
                "machineTranslated" -> machineTranslated,
                "votes" -> votes
            )
        }
    }

    implicit val WritesLabels = Writes[Map[Language, Seq[LabelTranslation]]] {
        case m => Json.toJson(for ((k,v) <- m) yield k.code -> v)
    }
    
    implicit val writesIRI = Writes[IRI] {
        case IRI(name, labels, id) => {
            Json.obj(
                "id" -> id,
                "name" -> name,
                "labels" -> Json.toJson(labels)
            )
        }
    }
}