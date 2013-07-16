package controllers

import anorm._

case class FormatField(id: Long, iriName : String, langName : String, label: String, format: String,votes: Long)