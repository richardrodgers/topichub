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

case class Finder(id: Long, scheme_id: Long, description: String, cardinality: String,
                  format: String, idKey: String, idLabel: String, author: String, created: Date)

object Finder {

  val finder = {
      get[Long]("id") ~ get[Long]("scheme_id") ~ get[String]("description") ~ get[String]("cardinality") ~ 
      get[String]("format") ~ get[String]("idKey") ~ get[String]("idLabel") ~ get[String]("author") ~ get[Date]("created") map {
      case id ~ scheme_id ~ description ~ cardinality ~ format ~ idKey ~ idLabel ~ author ~ created => 
        Finder(id, scheme_id, description, cardinality, format, idKey, idLabel, author, created)
    }
  }

  def create(scheme_id: Long, description: String, cardinality: String,
             format: String, idKey: String, idLabel: String, author: String) {
		DB.withConnection { implicit c =>
			SQL(
        """
        insert into finder (scheme_id, description, cardinality, format, idKey, idLabel, author, created)
        values ({scheme_id}, {description}, {cardinality}, {format}, {idKey}, {idLabel}, {author}, {created})
        """
      ).on('scheme_id -> scheme_id, 'description -> description, 'cardinality -> cardinality, 'format -> format, 'idKey ->idKey, 'idLabel -> idLabel, 'author -> author, 'created -> new Date)
      .executeUpdate()
		}
  }

  def findByScheme(scheme_id: Long): List[Finder] = {
    DB.withConnection { implicit c =>
      SQL("select * from finder where scheme_id = {scheme_id}").on('scheme_id -> scheme_id).as(finder *)
    }   
  }

  def forSchemeAndFormat(scheme_id: Long, format: String): Option[Finder] = {
    DB.withConnection { implicit c =>
      SQL("select * from finder where scheme_id = {scheme_id} and format = {format}")
      .on('scheme_id -> scheme_id,'format -> format).as(finder.singleOpt)
    }
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from finder where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
