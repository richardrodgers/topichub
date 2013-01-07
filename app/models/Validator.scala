/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package models

import java.util.Date

import play.api.db.DB
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL

/** Validators determine whether proposed identifiers are members of a scheme,
  * and return a canonical label if so. Typically, they invoke a web service.
  *
  * @author richardrodgers
  */

case class Validator(id: Long, scheme_id: Long, description: String, userId: String, password: String,
                     serviceCode: String, serviceUrl: String, author: String, created: Date)

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
