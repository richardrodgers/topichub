/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package controllers

import akka.actor.Props

import org.joda.time.format.ISODateTimeFormat

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.iteratee.Enumerator
import play.api.mvc._
import play.api.mvc.Security._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current

import models.{Subscriber, User}
import workers.{Conveyor, ConveyorWorker}

case class HubContext(user: Option[User])

trait Secured {
   def username(request: RequestHeader) = request.session.get("username")
   def onUnauthorized(request: RequestHeader) = Unauthorized("Not authorized")
   def onCouldAuthorize(request: RequestHeader) = Redirect(routes.AuthN.login).flashing("error" -> "Please login to perform this action")
   def isAuthenticated(f: => String => Request[AnyContent] => Result) = {
      Authenticated(username, onUnauthorized) { user =>
         Action(request => f(user)(request))
      }
   }
   def mustAuthenticate(f: => String => Request[AnyContent] => Result) = {
      Authenticated(username, onCouldAuthorize) { user =>
         Action(request => f(user)(request))
      }
   }
   implicit def hubContext[A](implicit request: Request[A]): HubContext = {
     request.session.get("username").map ( name =>
       HubContext(User.findByName(name))
     ).getOrElse(HubContext(None))
   }
}

object AuthN extends Controller with Secured {

  val hubNs = "http://topichub"
  val iso8601 = ISODateTimeFormat.dateTime

  val conveyor2 = Akka.system.actorOf(Props[ConveyorWorker], name="conveyor2")

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

  def logout = Action { implicit request =>
    Redirect(routes.Application.index).withNewSession
  }

  val forgotForm = Form(
    tuple(
      "username" -> text,
      "email" -> text
    ) verifying("Invalid Email address", result => true)
  )

  def forgot = Action { implicit request =>
    Ok(views.html.forgot(forgotForm))
  }

  def remind = Action { implicit request =>
      forgotForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.forgot(formWithErrors)),
      user => {
        val remUser = User.findByName(user._1).get
        val sub = Subscriber.findByUserId(remUser.id).get
        conveyor2 ! sub
        Redirect(routes.Application.index)
      }
    )    
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
        User.create(user._1, user._2, user._3, user._4)
        Redirect(routes.Application.index)
      }
    )
  }

}
