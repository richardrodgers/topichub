/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package models

import play.api.db.DB
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL

/** Ctype represents a content type.
  *
  * @author richardrodgers
  */

case class Ctype(id: Long, ctypeId: String, description: String, logo: Option[String]) {

  def addScheme(scheme: Scheme, relation: String) {
    DB.withConnection { implicit c =>
      SQL("insert into ctypescheme (ctype_id, scheme_id, relation) values ({ctype_id}, {scheme_id}, {relation})")
      .on('ctype_id -> id, 'scheme_id -> scheme.id, 'relation -> relation).executeUpdate()
    }
  } 

  def schemes(relation: String): List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select scheme.* from scheme, ctypescheme, ctype where scheme.id = ctypescheme.scheme_id and ctypescheme.ctype_id = ctype.id and ctype.id = {ctype_id} and ctypescheme.relation = {relation}")
      .on('ctype_id -> id, 'relation -> relation).as(Scheme.scheme *)
    }
  }

  def removeScheme(scheme: Scheme, relation: String) {
    DB.withConnection { implicit c =>
      SQL("delete from ctypescheme where ctype_id = {ctype_id} and scheme_id = {scheme_id} and relation = {relation}")
      .on('ctype_id -> id, 'scheme_id -> scheme.id, 'relation -> relation).executeUpdate()
    }  
  }
}

object Ctype {

  val ctype = {
    get[Long]("id") ~ get[String]("ctypeId") ~ get[String]("description") ~ get[String]("logo") map {
      case id ~ ctypeId ~ description ~ logo => Ctype(id, ctypeId, description, Some(logo))
    }
  }

  def create(ctypeId: String, description: String, logo: Option[String]) {
		DB.withConnection { implicit c =>
			SQL("insert into ctype (ctypeId, description, logo) values ({ctypeId}, {description}, {logo})")
      .on('ctypeId -> ctypeId, 'description -> description, 'logo  -> logo).executeUpdate()
		}
  }

  def all: List[Ctype] = {
    DB.withConnection { implicit c =>
      SQL("select * from ctype").as(ctype *)
    }   
  }

  def findById(id: Long): Option[Ctype] = {
    DB.withConnection { implicit c =>
      SQL("select * from ctype where id = {id}").on('id -> id).as(ctype.singleOpt)
    }
  }

  def findByName(ctypeId: String): Option[Ctype] = {
      DB.withConnection { implicit c =>
      SQL("select * from ctype where ctypeId = {ctypeId}").on('ctypeId -> ctypeId).as(ctype.singleOpt)
    }  
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
      SQL("delete from ctypescheme where ctype_id = {ctype_id}").on('ctype_id -> id).executeUpdate()
  		SQL("delete from ctype where id = {id}").on('id -> id).executeUpdate()
  	}
  }

  def mapView: Map[String, String] = {
      all map (ct => ct.id.toString -> ct.ctypeId) toMap
  }
}
