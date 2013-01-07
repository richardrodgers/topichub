/**
  * Copyright 2012 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import java.util.Date

import play.api.db.DB
import play.api._
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

case class Publisher(id: Long, userId: Long, pubId: String, name: String, description: String, category: String,
                     status: String, link: Option[String], logo: Option[String], created: Date) {

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
    get[Long]("id") ~ get[Long]("user_id") ~ get[String]("pub_id") ~ get[String]("name") ~ get[String]("description") ~ get[String]("category") ~
    get[String]("status") ~ get[Option[String]]("link") ~ get[Option[String]]("logo") ~ get[Date]("created") map {
      case id ~ userId ~ pubId ~ name ~ description ~ category ~ status ~ link ~ logo ~ created => 
        Publisher(id, userId, pubId, name, description, category, status, link, logo, created)
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

  def categories = {
    DB.withConnection { implicit c =>
      SQL("select distinct category from publisher").as(scalar[String] *)
    }
  }

  def categoryCount(category: String) = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from publisher where category = {category}").on('category -> category).apply.head
      count[Long]("c")
    }
  }

  def inCategory(category: String, page: Int) = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from publisher where category = {category}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('category -> category, 'offset -> offset).as(pub *)
    }  
  }

  def create(userId: Long, pubId: String, name: String, description: String, category: String, status: String, link: Option[String], logo: Option[String]) = {
    DB.withConnection { implicit c =>
      SQL("insert into publisher (user_id, pub_id, name, description, category, status, link, logo, created) values ({user_id}, {pubId}, {name}, {description}, {category}, {status}, {link}, {logo}, {created})")
      .on('user_id -> userId, 'pubId -> pubId, 'name -> name, 'description -> description, 'category -> category, 'status -> status, 'link -> link, 'logo -> logo, 'created -> new Date).executeInsert()
    }
  }

  def make(userId: Long, pubId: String, name: String, description: String, category: String, status: String, link: Option[String], logo: Option[String]) = {
    findById(create(userId, pubId, name, description, category, status, link, logo).get).get
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

  def interpolate(token: String) = {
    val start = token.indexOf("${")
    if (start >= 0) {
      val end = token.indexOf("}", start + 2)
      Play.configuration.getString(token.substring(start + 2, end)).get
    } else
      token
  }

}
