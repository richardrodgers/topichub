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
import anorm.SqlRow

/** Publisher is a provider of content to a hub, although it may not be the content originator:
  * it may be only represent a forwarding agent/proxy (e.g. another hub). Content is organized
  * into collections.
  *
  * @author richardrodgers
  */

case class Publisher(id: Long, pubId: String, name: String, description: String, role: String,
                     home: Option[String], logo: Option[String], created: Date) {

  def collectionCount = {
     DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from collection where publisher_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def collections = {
     DB.withConnection { implicit c =>
      SQL("select * from collection where publisher_id = {pubId}").on('pubId -> id).as(Collection.coll *)
    }
  }

  def itemCount = {
    DB.withConnection { implicit c =>
      if (collectionCount > 0L) {
        val sum = SQL("select sum(deposits) as sm from collection where publisher_id = {id}").on('id -> id).apply.head
        sum match {
          case sum: SqlRow => sum[Long]("sm")
          case _ => 0
        }
      } else 0
    }
  }
}

object Publisher {

  val pub = {
    get[Long]("id") ~ get[String]("pubId") ~ get[String]("name") ~ get[String]("description") ~ get[String]("role") ~
    get[Option[String]]("home") ~ get[Option[String]]("logo") ~ get[Date]("created") map {
      case id ~ pubId ~ name ~ description ~ role ~ home ~ logo ~ created => 
        Publisher(id, pubId, name, description, role, home, logo, created)
    }
  }

  def findById(id: Long): Option[Publisher] = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher where id = {id}").on('id -> id).as(pub.singleOpt)
    }
  }

  def all: List[Publisher] = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher").as(pub *)
    }
  }

  def create(pubId: String, name: String, description: String, role: String, home: Option[String], logo: Option[String]) {
    DB.withConnection { implicit c =>
      SQL("insert into publisher (pubId, name, description, role, home, logo, created) values ({pubId}, {name}, {description}, {role}, {home}, {logo}, {created})")
      .on('pubId -> pubId, 'name -> name, 'description -> description, 'role -> role, 'home -> home, 'logo -> logo, 'created -> new Date).executeUpdate()
    }
  }

  def delete(id: Long) {
    DB.withConnection { implicit c =>
      SQL("delete from publisher where id = {id}").on('id -> id).executeUpdate()
    }
  }
}

// belongs elsewhere - but model package automatically in scope in templates, so this is visible w/o imports
object HubUtils {

  import org.joda.time.format.ISODateTimeFormat

  val iso8601 = ISODateTimeFormat.date

  def fmtDate(date: Date) = {
    iso8601.print(date.getTime())
  }

  def pluralize(amt: Int, word: String): String = pluralize(amt.toLong, word)

  def pluralize(amt: Long, word: String) = {
    amt match {
      case 0L => "No " + word + "s"
      case 1L => "One " + word
      case _ => amt + " " + word + "s"
    }
  }

}
