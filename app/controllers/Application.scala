package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import models._
import play.api.i18n._
import anorm._
import scala.io.Source
import views.html.defaultpages.badRequest
import views.html._

object Application extends Controller with Secured {

  var languages: Seq[Lang] = Seq()
  implicit val flash = new play.api.mvc.Flash(Map(("message", "")))
  var count = 0
  var listLangs = Language.all()


  def index = Action { implicit request =>
    languages = request.acceptLanguages

    if (count == 0) {
      initialize()
    }
    Ok(views.html.index(listLangs, languages, searchForm))
    
  }

  //Only the first time application runs
  private def initialize() {
    loadLanguages()
    loadTrans()
    count += 1
  }

  /**
   * Load Languages from file
   * @param f
   * @return
   */
  private def loadLanguages() {
    val s = io.Source.fromFile("doc/langs.txt", "UTF-8")
    s.getLines.foreach((line) => {
      val array = line.split(" ")
      if (array.length == 3)
        Language.create(array(0), array(1) + "-" + array(2))
      else Language.create(array(0), array(1))

    })
  }

  //Delete all the translations and load default
  private def loadTrans() {
    Translation.deleteALL()
    val s = Source.fromFile("doc/trans.txt", "UTF-8")
    s.getLines.foreach((line) => {
      val array = line.split("\t")
      val lang = array(1).split("-");
      IRI.create(array(0))
      Admin.newDefaultTranslation(array(0), "en", lang(0), 1)
      Admin.newDefaultTranslation(array(0), "es", lang(1), 1)
    })
  }

  //Redirect to about page
  def about = Action { implicit request =>
    Ok(views.html.about())
  }
    def aboutFormats = Action { implicit request =>
    Ok(views.html.aboutFormats())
  }
      def aboutAPI = Action { implicit request =>
    Ok(views.html.aboutAPI())
  }

  def format = Action { implicit request =>
    formatForm.bindFromRequest.fold(
      errors => BadRequest("Error:" + errors.toString()),
      formatField => {
        val viewTrans = ViewTranslation(formatField.id, formatField.iriName, formatField.langName, formatField.label, formatField.votes)
        contentNegotiation(Some(formatField.format)) match {
          case TURTLE() => {
            Ok(views.html.format(getTurtle(viewTrans),formatField.format))	
          }
          case XML() => {
            Ok(views.html.format(getXML(viewTrans),formatField.format))
          }
          case TXT() => {
            Ok(views.html.format(getTxt(viewTrans),formatField.format))
          }
          case N3() => {
            Ok(views.html.format(getN3(viewTrans),formatField.format))
          }
          case JSON() => {
            Ok(views.html.format(getJson(viewTrans),formatField.format))
          }
          case ERROR() => prepareError("error")
        }
      })
  }

  //Search a translation from index page
  def searchTranslation = Action { implicit request =>
    searchForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(listLangs, languages, errors)),
      searchField => {
        val iriName = searchField.iriName
        val langName = searchField.langName
        Language.lookup(langName) match {
        	case None => Ok(views.html.errors.errorLang(langName))
        	case Some(langId) =>{	
	        val result = Translation.lookupTranslation(iriName, langName)
	        result match {
	          case None =>
	            val iri = IRI.lookup(iriName)
	            val viewTrans = ViewTranslation(0, iriName, langName, "", 0)
	            iri match {
	              case None =>    Ok(views.html.errors.errorTrans(viewTrans))
	              case Some(iriId) => Ok(views.html.errors.errorTrans(viewTrans))
	            }
	          case Some(trans) =>
	            val viewTrans = ViewTranslation(trans.id.get, iriName, langName, trans.transLabel, trans.votes)
	            prepareHTML(viewTrans)
	        }}}
      })
  }

  //Get the result page with the web service
  def getTranslation(langCode: String) = Action { implicit request =>
    val iri = request.queryString.get("iri").flatMap(_.headOption).getOrElse("")
    var format = request.queryString.get("format").flatMap(_.headOption).getOrElse("")
    if(format.length==0){
      format = "text/html"
    }
    println("format:"+format)
    Language.lookup(langCode) match {
      case None => Ok("Language " + langCode + " not found.")
      case Some(langId) =>
        IRI.lookup(iri) match {
          case None => Ok("IRI " + iri + " not found.")
          case Some(iriId) =>
            val result = Translation.lookupTranslation(iri, langCode)
            result match {
              case None => NotFound("Translation not found for \""+iri+"\"@"+langCode)
              case Some(trans) =>
                val viewTrans = ViewTranslation(trans.id.get, iri, langCode, trans.transLabel, trans.votes)
                contentNegotiationService(format) match {
                  case HTML() => prepareHTML(viewTrans)
                  case TURTLE() => prepareTurtle(viewTrans)
                  case XML() => prepareXML(viewTrans)
                  case TXT() => prepareTxt(viewTrans)
                  case N3() => prepareN3(viewTrans)
                  case JSON() => prepareJson(viewTrans)
                  case ERROR() => Ok("Format " + format + " not found.")
                }
            }
        }
    }
  }

  //Vote the translation selected
  def vote(id: Long, vote: String) = Action { implicit request =>
    Translation.findById(id) match {
      case None => Ok(views.html.errors.error())
      case Some(trans) => {
        var value = trans.votes
        if (vote.equals("neg")) {
          value = value - 1;
        } else {
          value = value + 1;
        }
        Translation.updateVotes(id, value)
      }
    }
    Ok(vote)
  }
  //Redirect to new Trans page with values
  def addTrans(iri:String, langCode:String) = Action { implicit request =>
	implicit val flash = new play.api.mvc.Flash(Map(("iri", iri),("lang",langCode)))
    Ok(views.html.newTranslation(listLangs, translationForm))
  }
    def newAltTrans(iri:String, langCode:String) = Action { implicit request =>
	implicit val flash = new play.api.mvc.Flash(Map(("iri", iri),("lang",langCode)))
    Ok(views.html.addTranslation(translationForm))
  }
  //Redirect to new Trans page
  def newTrans() = Action { implicit request =>
    Ok(views.html.newTranslation(listLangs, translationForm))
  }

  //Load langs with ajax for datalist in index page
  def loadLangs(iri: String) = Action { implicit request =>
    var s = "";
    IRI.lookup(iri) match {
      case None => Ok("IRI " + iri + " not found. Create IRI before")
      case Some(iriId) => {
        var list = Translation.lookupLangs(iriId)
        if (list != null) {
          list.foreach(lang => {
            Language.findLangCode(lang) match {
              case None => Ok("Lang " + lang + " not found.")
              case Some(iriName) => {
                s = s + iriName
                s = s + ","
              }
            }
          })
        }
      }
    }
    Ok(s)
  }
  def viewAlternatives(iriId: Long, idLang: Long): List[ViewTranslation] = {
    Translation.alt(iriId, idLang).map(t => ViewTranslation(t.id.get,
      IRI.findIRIName(t.iriId).getOrElse("Not found"),
      Language.findLangCode(t.langId).getOrElse("Not found"),
      t.transLabel,
      t.votes.toInt))
  }

  def alternative = Action { implicit request =>
    val iri = request.queryString.get("iriName").flatMap(_.headOption).getOrElse("")
    val id = request.queryString.get("id").flatMap(_.headOption).getOrElse("")
    val langCode = request.queryString.get("langCode").flatMap(_.headOption).getOrElse("")
    val label = request.queryString.get("label").flatMap(_.headOption).getOrElse("")
    val votes = request.queryString.get("votes").flatMap(_.headOption).getOrElse("")

    val iriId = IRI.lookup(iri)
    iriId match {
      case None => Ok(views.html.errors.error())
      case Some(idIri) => {
        val langId = Language.lookup(langCode)
        langId match {
          case None => Ok(views.html.errors.errorLang(langCode))
          case Some(idLang) => {
            Ok(views.html.alternatives(viewAlternatives(idIri, idLang),iri,langCode))
          }
        }
      }
    }
  }

  def showTrans(id: Long) = Action {
    val translation = Translation.findById(id)
    translation match {
      case None => Ok(views.html.errors.error())
      case Some(trans) => {
        IRI.findIRIName(trans.iriId) match {
        case None => Ok(views.html.errors.error())  
        case Some(iriName) =>
            Language.findLangCode(trans.langId) match {
              case None => Ok(views.html.errors.error())
              case Some(langCode) =>
                val viewTrans = ViewTranslation(trans.id.get, iriName, langCode, trans.transLabel, trans.votes)
                prepareHTML(viewTrans)
            }
        }
      }
    }
  }

  sealed class Format
  case class HTML() extends Format
  case class TURTLE() extends Format
  case class JSON() extends Format
  case class XML() extends Format
  case class N3() extends Format
  case class TXT() extends Format
  case class ERROR() extends Format


  private def contentNegotiation(format: Option[String]): Format = {
    format match {
      case Some("text/turtle") => TURTLE()
      case Some("text/n3") => N3()
      case Some("text/xml") => XML()
      case Some("text/txt") => TXT()
      case Some("text/html") => HTML()
      case Some("application/json") => JSON()
      case _ => ERROR()
    }
  }

  private def contentNegotiationService(format: String): Format = {
    format match {
      case ("text/turtle") => TURTLE()
      case ("text/n3") => N3()
      case ("text/xml") => XML()
      case ("text/txt") => TXT()
      case ("text/html") => HTML()
      case ("application/json") => JSON()
      case _ => ERROR()
    }
  }
  //Encoding for N3
  private def encoding(cad: String): String = {
    var s = ""
    var i = 0;
    for (i <- 0 until cad.length) {
      if (cad(i).toInt < 127) {
        s = s + cad(i)
      } else {
        s = s + "\\u%04X".format(cad(i).toInt)
      }
    }
    s
  }

  val rdfslabel = "http://www.w3.org/2000/01/rdf-schema#label"

  private def prepareN3(viewTrans: ViewTranslation) = {
    Ok("<" + viewTrans.iri + "> <" + rdfslabel + "> \"" + encoding(viewTrans.label) + "\"@" + viewTrans.langCode + " .")
  }
  private def getN3(viewTrans: ViewTranslation) : String={
    return ("<" + viewTrans.iri + "> <" + rdfslabel + "> \"" + encoding(viewTrans.label) + "\"@" + viewTrans.langCode + " .")
  }

  private def prepareTxt(viewTrans: ViewTranslation) = {
    Ok(viewTrans.iri + "  \"" + viewTrans.label + "\"@" + viewTrans.langCode + " .")
  }
  private def getTxt(viewTrans: ViewTranslation):String = {
    return(viewTrans.iri + "  \"" + viewTrans.label + "\"@" + viewTrans.langCode + " .")
  }

 private def prepareHTML(viewTrans: ViewTranslation) = {
    Ok(views.html.result(viewTrans, formatForm))
  }

  private def prepareTurtle(viewTrans: ViewTranslation) = {
    Ok("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n" +
      "<" + viewTrans.iri + "> <" + rdfslabel + "> \"" + viewTrans.label + "\"@" + viewTrans.langCode + " .")
  }
   
  private def getTurtle(viewTrans: ViewTranslation):String = {
    return("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n" +
      "<" + viewTrans.iri + "> <" + rdfslabel + "> \"" + viewTrans.label + "\"@" + viewTrans.langCode + " .")
  }

  private def prepareJson(viewTrans: ViewTranslation) = {
    Ok("{ iri: " + viewTrans.iri + "," +
      " label: " + viewTrans.label + "," +
      " language: " + viewTrans.langCode +
      " }")
  }
  private  def getJson(viewTrans: ViewTranslation) : String = {
    return("{ iri: " + viewTrans.iri + "," +
      " label: " + viewTrans.label + "," +
      " language: " + viewTrans.langCode +
      " }")
  }

  private def prepareXML(viewTrans: ViewTranslation) = {
    Ok("<rdf:RDF\n xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" +
      "    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\" >\n" +
      " <rdf:Description rdf:about= \"" + viewTrans.iri + "\">\n" +
      "    <rdfs:label xml:lang=\"" + viewTrans.langCode + "\">" + viewTrans.label + "</rdfs:label>\n" +
      " </rdf:Description>\n" +
      "</rdf:RDF>")
  }
  private  def getXML(viewTrans: ViewTranslation) :String = {
    return("<rdf:RDF\n xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" +
      "    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\" >\n" +
      " <rdf:Description rdf:about= \"" + viewTrans.iri + "\">\n" +
      "    <rdfs:label xml:lang=\"" + viewTrans.langCode + "\">" + viewTrans.label + "</rdfs:label>\n" +
      " </rdf:Description>\n" +
      "</rdf:RDF>")
  }
  
  private def prepareError(msg: String) = {
        Ok(views.html.errors.errorFormat(msg))
  }

  //routing for ajax
  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsRoutes")(routes.javascript.Application.loadLangs, routes.javascript.Application.vote)).as("text/javascript")
  }

  val formatForm: Form[FormatField] = Form(
    mapping(
      "id" -> of[Long],
      "iriName" -> text.verifying(Messages("error.iri"), {!_.isEmpty}),
      "langCode" -> text.verifying(Messages("error.lang"), {!_.isEmpty}),
      "label" -> text.verifying(Messages("error.label"), {!_.isEmpty}),
      "format" -> nonEmptyText,
      "votes" -> of[Long])(FormatField.apply)(FormatField.unapply))

  val searchForm: Form[SearchField] = Form(
    mapping(
      "iriName" -> text.verifying(Messages("error.iri"), {!_.isEmpty}),
      "langCode" -> text.verifying(Messages("error.lang"), {!_.isEmpty}))(SearchField.apply)(SearchField.unapply))

  val translationForm: Form[ViewTranslation] = Form(
    mapping(
      "id" -> of[Long],
      "iri" -> text.verifying(Messages("error.iri"), {!_.isEmpty}),
      "langCode" -> text.verifying(Messages("error.lang"), {!_.isEmpty}),
      "label" -> text.verifying(Messages("error.label"), {!_.isEmpty}),
      "votes" -> of[Long])(ViewTranslation.apply)(ViewTranslation.unapply))

}