package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import models._
import play.api.i18n._
import anorm._

object Admin extends Controller {


  def admin = Action { implicit request =>
    if (request.session.get(Security.username) != None) {
      Ok(views.html.admin())
    } else
      Ok(views.html.login(Auth.loginForm))
  }

  def newIRI = Action { implicit request =>
    iriForm.bindFromRequest.fold(
      errors => Ok("Errors" + errors), // BadRequest(views.html.index(searchForm)),
      iriName => {
        if (IRI.lookup(iriName) == None){
        	IRI.create(iriName)
        	Redirect(routes.Admin.iris)}
        else Ok(views.html.errors.errorDuplicateIRI())
      })
  }
  
  def addIRI = Action { implicit request =>
    if (request.session.get(Security.username) != None) {
      Ok(views.html.newIri(iriForm))
    } else
      Ok(views.html.login(Auth.loginForm))
  
  }

  def newLang = Action { implicit request =>
    langForm.bindFromRequest.fold(
      errors => Ok("Error " + errors.toString()), // BadRequest(views.html.index(Language.all(), errors)),
      language => {
        Language.insert(language)
        Redirect(routes.Admin.languages)
      })
  }

  def newTranslation = Action {
    implicit request =>
      translationForm.bindFromRequest.fold(
        errors => BadRequest(views.html.newTranslation(Application.listLangs, errors)),
          //Ok("Error " + errors.toString()), // BadRequest(views.html.index(Trans.all(), errors)),
        vt => {
          Language.lookup(vt.langCode) match {
            case None => Ok(views.html.errors.errorLang(vt.langCode+" "+Messages("ErrorLang")))
            case Some(langId) =>
              var found = false
              IRI.lookup(vt.iri) match {
                case None =>
                  IRI.create(vt.iri)
                  IRI.lookup(vt.iri) match {
                    case None =>
                      Ok("Error. Iri: " + vt.iri + " can not be created.")
                    case Some(iriId) =>
                      Translation.create(iriId, langId, vt.label, vt.votes)
                      Redirect(routes.Application.index)
                  }
                case Some(iriId) =>
                  val listTrans = Translation.alt(iriId, langId)
                  listTrans.map(t =>
                        if(t.transLabel==vt.label){found = true;}                        
                        )
                  if(found){
                    Ok(views.html.errors.errorDuplicateTrans())
                  }
                  else{
                	  Translation.create(iriId, langId, vt.label, vt.votes)
                  	Redirect(routes.Application.index)
                  }
              }
          }
        })
  }
  
  def newAlternativeTranslation = Action {
    implicit request =>
      translationForm.bindFromRequest.fold(
        errors => BadRequest(views.html.newTranslation(Application.listLangs, errors)),
          //Ok("Error " + errors.toString()), // BadRequest(views.html.index(Trans.all(), errors)),
        vt => {
          Language.lookup(vt.langCode) match {
            case None => Ok(views.html.errors.errorLang(vt.langCode+" "+Messages("ErrorLang")))
            case Some(langId) =>
              IRI.lookup(vt.iri) match {
                case None =>
                  Ok("Error. Iri: " + vt.iri + " can not be created.") 
                case Some(iriId) =>
                  var found = false
                  val listTrans = Translation.alt(iriId, langId)
                  listTrans.map(t =>
                        if(t.transLabel==vt.label){found = true;}                        
                        )
                  if(found){
                    Ok(views.html.errors.errorDuplicateTrans())
                  }
                  else{
                	  Translation.create(iriId, langId, vt.label, vt.votes)
                  	Ok(views.html.alternatives(Application.viewAlternatives(iriId, langId),vt.iri,vt.langCode))
                  }
              }
          }
        })
  }
  //para cargar las traducciones por defecto
  def newDefaultTranslation(iri: String, langCode: String, transLabel: String, votes: Long) {
    Language.lookup(langCode) match {
      case None => Ok("Language " + langCode + " not found.")
      case Some(langId) =>
        IRI.lookup(iri) match {
          case None => Ok("IRI " + iri + " not found. Create IRI before")
          case Some(iriId) => {
            Translation.create(iriId, langId, transLabel, 1)
          }
        }
    }
  }

  def deleteIRI(id: Long) = Action { implicit request =>
    if (request.session.get(Security.username) != None){
      var list = Translation.lookupLangs(id) 
      if(list.length  == 0){
    	  IRI.delete(Id(id))      
    	   Redirect(routes.Admin.iris)
      }
      else Ok(views.html.errors.errorIRI())     
    }
    else Ok(views.html.errors.errorLang("You don't have permission for this action"))
  }

  def deleteLang(id: Long) = Action {implicit request =>
    if (request.session.get(Security.username) != None){ 
    	Language.delete(Id(id))
    	Redirect(routes.Admin.languages)
    }
    else Ok(views.html.errors.errorLang("You don't have permission for this action"))
  }

  def deleteTrans(id: Long) = Action { implicit request =>
    if (request.session.get(Security.username) != None){ 
	    Translation.delete(Id(id))
	    Redirect(routes.Admin.translations)
    }
    else Ok(views.html.errors.errorLang("You don't have permission for this action"))
  }

  val iriForm: Form[String] = Form(
    "iriName" -> text.verifying(Messages("error.iri"), {!_.isEmpty}))

  val langForm: Form[Language] = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "langCode" -> text,
      "langName" -> text)(Language.apply)(Language.unapply))

  val translationForm: Form[ViewTranslation] = Form(
    mapping(
      "id" -> of[Long],
      "iri" -> text.verifying(Messages("error.iri"), {!_.isEmpty}),
      "langCode" -> text.verifying(Messages("error.lang"), {!_.isEmpty}),
      "label" -> text.verifying(Messages("error.label"), {!_.isEmpty}),
      "votes" -> of[Long])(ViewTranslation.apply)(ViewTranslation.unapply))

  def iris = Action { implicit request =>
    if (request.session.get(Security.username) == None) {
      Redirect(routes.Auth.login)
    } else
      Ok(views.html.iris(IRI.all(), iriForm))

  }

  def languages = Action { implicit request =>
    if (request.session.get(Security.username) == None) {
      Redirect(routes.Auth.login)
    } else {
      Ok(views.html.languages(Language.all(), langForm, Lang("es")))
    }
  }

  def viewTranslations: List[ViewTranslation] = {
    Translation.all().map(t => ViewTranslation(t.id.get,
      IRI.findIRIName(t.iriId).getOrElse("Not found"),
      Language.findLangCode(t.langId).getOrElse("Not found"),
      t.transLabel,
      t.votes.toInt))
  }

  def translations = Action { implicit request =>
    if (request.session.get(Security.username) == None) {
      Redirect(routes.Auth.login)
    } else {
      Ok(views.html.translations(viewTranslations, translationForm, Lang("es")))
    }
  }
}