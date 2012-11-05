/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package controllers

import scala.xml.{NodeSeq, XML}

import org.joda.time.format.ISODateTimeFormat

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.iteratee.Enumerator
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current

import models.Subscriber

object Security extends Controller {

  val hubNs = "http://topichub"
  val iso8601 = ISODateTimeFormat.dateTime

  val loginForm = Form(
    tuple(
      "email" -> text,
      "password" -> text
    ) verifying ("Invalid email or password", result => result match {
      case (email, password) => Subscriber.authenticate(email, password).isDefined
    })
  )
  /** Returns a login form for unauthenticated user
    *
    */
  def login = Action { implicit request =>
    Ok(views.html.login(loginForm))
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors)),
      user => Redirect(routes.Application.index).withSession("email" -> user._1)
    )    
  }

}
