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

import models.{Subscriber, User}

object Security extends Controller {

  val hubNs = "http://topichub"
  val iso8601 = ISODateTimeFormat.dateTime

  val loginForm = Form(
    tuple(
      "username" -> text,
      "password" -> text
    ) verifying ("Invalid username or password", result => result match {
      case (username, password) => User.authenticate(username, password).isDefined
    })
  )
  /** Returns a login form for unauthenticated user
    *
    */
  def login = Action { implicit request =>
    Ok(views.html.login(loginForm))
  }

  def logout = Action {
    Ok(views.html.index("hello")).withNewSession
  }

  val forgotForm = Form(
    tuple(
      "email" -> text,
      "again" -> text
    ) verifying("Invalid Email address", result => true)
  )

  def forgot = Action { implicit request =>
    Ok(views.html.forgot(forgotForm))
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors)),
      user => Redirect(routes.Application.index).withSession("username" -> user._1)
    )    
  }

  val regForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "email" -> nonEmptyText, 
      "password" -> nonEmptyText,
      "role" -> nonEmptyText
    ) verifying ("Invalid userName - choose another", result => result match {
      case user => User.findByName(user._1).isEmpty
    })
  )

  /**
    * Presents a registration form
    *
    */
  def startregister = Action { implicit request =>
    Ok(views.html.register(regForm))
  }

  def register = Action { implicit request =>
    regForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.register(formWithErrors)),
      user => {
        User.create(user._1, user._2, user._3, "admin")
        Redirect(routes.Application.index)
      }
    )
  }

}
