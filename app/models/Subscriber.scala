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

/**
  * Subscriber is an entity that receives notificatons and/or content deliveries based on hub-registered intents.
  *
  * @author richardrodgers
  */

case class Subscriber(id: Long, userId: String, password: String, home: Option[String], logo: Option[String], role: String,
                      contact: String, swordService: String, terms: String, backFile: String, created: Date) {

  def targets = {
   DB.withConnection { implicit c =>
      SQL("select * from target where target.subscriber_id = {id}")
      .on('id -> id).as(Target.target *)
    }
  }
}

object Subscriber {

  val subscriber = {
    get[Long]("id") ~ get[String]("userId") ~ get[String]("password") ~ get[String]("home") ~ get[String]("logo") ~ get[String]("role") ~
    get[String]("contact") ~ get[String]("swordService") ~ get[String]("terms") ~ get[String]("backfile") ~ get[Date]("created") map {
      case id ~ userId ~ password ~ home ~ logo ~ role ~ contact ~ swordService ~ terms ~ backFile ~ created => 
      Subscriber(id, userId, password, Some(home), Some(logo), role, contact, swordService, terms, backFile, created)
    }
  }

  def all: List[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber").as(subscriber *)
    }   
  }

  def count = {
     DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from subscriber").apply.head
      count[Long]("c")
    }
  }

  def findById(id: Long): Option[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where id = {id}").on('id -> id).as(subscriber.singleOpt)
    }
  }

  def create(userId: String, password: String, home: Option[String], logo: Option[String], role: String, contact: String,
             swordService: String, terms: String, backFile: String) {
		DB.withConnection { implicit c =>
			SQL("insert into subscriber (userId, password, home, logo, role, contact, swordService, terms, backFile) values ({userId}, {password}, {home}, {logo}, {role}, {contact}, {swordService}, {terms}, {backFile})")
      .on('userId -> userId, 'password -> password, 'home -> home, 'logo -> logo, 'role -> role, 'contact -> contact, 'swordService -> swordService, 'terms -> terms, 'backFile -> backFile).executeUpdate()
		}
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from subscriber where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}