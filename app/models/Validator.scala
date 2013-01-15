/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package models

import java.util.Date

import play.api.db.DB
import play.api.Play.current
import play.api.libs.concurrent._
import play.api.libs.ws.WS
import play.api.libs.ws._
import play.api.mvc._
import play.api.libs.json._
import play.api.http.HeaderNames._

import anorm.SqlParser._
import anorm.~
import anorm.SQL

import HubUtils._

/** Validators determine whether proposed identifiers are members of a scheme,
  * and return a canonical label if so. Typically, they invoke a web service,
  * but could just do syntactic analysis, etc
  *
  * @author richardrodgers
  */

case class Validator(id: Long, scheme_id: Long, description: String, userId: String, password: String,
                     serviceCode: String, serviceUrl: String, author: String, created: Date) {

  def validate(topicId: String): Either[String, String] = { 
    val scheme = Scheme.findById(scheme_id).get
    // compose the service call
    val svcCall = interpolate(serviceUrl.replaceAll("\\{topic\\}", topicId), true)
    val req = WS.url(svcCall)
    //println("svcCall: " + svcCall)
    // need to define general abstraction - now just key off schemeId
    if ("issn".equals(scheme.schemeId)) {
      val resp = req.get
      val json = resp.await.get.json
      issnParse(json)
    } else if ("orcid".equals(scheme.schemeId)) {
      val resp = req.withHeaders(ACCEPT -> "application/orcid+json").get
      val json = resp.await.get.json
      orcidParse(json)
    } else {
      Left("Not implemented")
    }
  }

  private def orcidParse(json: JsValue): Either[String, String] = {
    (json \ "orcid-profile") match {
      case undef: JsUndefined => Left("Not found")
      case _ => {
        // compose name
        val full = ((json \\ "given-names").head \ "value").as[String] + " " +
                   ((json \\ "family-name").head \ "value").as[String]
        Right(full)
      }
    } 
  }

  private def issnParse(json: JsValue): Either[String, String] = {
    val stat = (json \ "stat").as[String]
    stat match {
      case "ok" => Right((json \\ "title").head.as[String])
      case _ => Left(stat)
    }
  }
}

object Validator {

  val validator = {
      get[Long]("id") ~ get[Long]("scheme_id") ~ get[String]("description") ~ get[String]("user_id") ~ get[String]("password") ~
      get[String]("service_code") ~ get[String]("service_url") ~ get[String]("author") ~ get[Date]("created") map {
      case id ~ scheme_id ~ description ~ userId ~ password ~ serviceCode ~ serviceUrl ~ author ~ created => 
        Validator(id, scheme_id, description, userId, password, serviceCode, serviceUrl, author, created)
    }
  }

  def create(scheme_id: Long, description: String, userId: String, password: String,
             serviceCode: String, serviceUrl: String, author: String) {
		DB.withConnection { implicit c =>
			SQL(
        """
        insert into validator (scheme_id, description, user_id, password, service_code, service_url, author, created)
        values ({scheme_id}, {description}, {user_id}, {password}, {service_code}, {service_url}, {author}, {created})
        """
      ).on('scheme_id -> scheme_id, 'description -> description, 'user_id -> userId, 'password -> password, 'service_code -> serviceCode, 'service_url -> serviceUrl, 'author -> author, 'created -> new Date)
      .executeUpdate()
		}
  }

  def findByScheme(scheme_id: Long): List[Validator] = {
    DB.withConnection { implicit c =>
      SQL("select * from validator where scheme_id = {scheme_id}").on('scheme_id -> scheme_id).as(validator *)
    }   
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from validator where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
