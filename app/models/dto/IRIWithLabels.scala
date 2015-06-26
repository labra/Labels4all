package models.dto

import scala.xml.XML
import scala.xml.Node
import scala.util.control.NonFatal

import PartialFunction.condOpt
import org.apache.commons.lang3.StringEscapeUtils

object Namespace {
    val RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
    val RDFS = "http://www.w3.org/2000/01/rdf-schema#"
    val OWL = "http://www.w3.org/2002/07/owl#" 
    val DC = "http://purl.org/dc/elements/1.1/"
    val XML = "http://www.w3.org/XML/1998/namespace"
}

case class IRIWithLabels(name: String, labels: Map[String, Seq[String]])

object IRIWithLabels {
    
    def fromXML(xmlStr: String, lang: Option[String]): Option[Seq[IRIWithLabels]] = {
        try {
            val unescapedXml = StringEscapeUtils.unescapeXml(xmlStr)
            val xmlObj = XML.loadString(unescapedXml)
            val baseAttr = xmlObj.attribute(Namespace.XML, "base")
            val base = baseAttr match {
                case Some(b) => b.toString
                case None => ""
            }
            Some((for(node <- xmlObj.child) yield fromXMLNode(node, base, lang))
                     .flatten.filter(!_.labels.isEmpty))
        } catch {
            case NonFatal(ex) => None
        }
    }
    
    private[this] def fromXMLNode(node: Node, base: String, lang: Option[String]): Option[IRIWithLabels] = {
        val aboutAttr = node.attribute(Namespace.RDF, "about")
        condOpt(aboutAttr) {
            case Some(about) => {
                val aboutStr = about.toString
                val IRI = 
                    if(aboutStr.startsWith("#")) 
                        base + aboutStr
                    else 
                        aboutStr
                
                val labelAttr = node.attribute(Namespace.RDFS, "label")
                val label = condOpt(labelAttr) {
                    case Some(lb) => lb.toString
                }
                
                label match {
                    case Some(s) => {
                        lang match {
                            case Some(dl) => IRIWithLabels(IRI, Map(dl -> Seq(s)))
                            case None => throw new Exception("Missing default language")
                        }
                    }
                    case None => {
                        if(node.child.isEmpty)
                            IRIWithLabels(IRI, Map())
                        else {
                            val labels = node.child.filter(n => ((n.namespace == Namespace.RDFS && n.label == "label")))
                            
                            var languages: Map[String, Seq[String]] = Map()
                            for(labelNode <- labels) {
                                var text = labelNode.text
                                val langAttr = labelNode.attribute(Namespace.XML, "lang")
                                val language = langAttr match {
                                    case Some(la) => la.toString
                                    case None => lang match {
                                        case Some(dl) => dl
                                        case None => throw new Exception("Missing default language")
                                    }
                                }
                            
                                if(languages.contains(language))
                                    languages(language) :+ text
                                else
                                    languages += language -> Seq(text)
                            }
                            
                            IRIWithLabels(IRI, languages)
                        }
                    }
                }
            }
        }
    }
}