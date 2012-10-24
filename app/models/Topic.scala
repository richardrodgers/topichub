/**
 * Copyright 2012 MIT Libraries
 * Licensed under:  http://www.apache.org/licenses/LICENSE-2.0
 */
package models

import java.util.Date

import play.api.db.DB
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL

/** Topic represents a value in a namespace (its scheme). It is the object of subscriptions
  *
  * @author richardrodgers
  */

case class Topic(id: Long, scheme_id: Long, topicId: String, title: String,
                 created: Date, updated: Date, transfers: Int) {

  def items = {
   DB.withConnection { implicit c =>
      SQL("select item.* from item, itemtopic where item.id = itemtopic.item_id and itemtopic.topic_id = {topic_id}")
      .on('topic_id -> id).as(Item.item *)
    }
  }

   def itemCount = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from itemtopic where topic_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def scheme = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme where id = {scheme_id}")
      .on('scheme_id -> scheme_id).as(Scheme.scheme.singleOpt)
    }   
  }

  def subscriptionCount = {
     DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from subscription where topic_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }
}

object Topic {

  val topic = {
    get[Long]("id") ~ get[Long]("scheme_id") ~ get[String]("topicId") ~ get[String]("title") ~ 
    get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~ scheme_id ~ topicId ~ title ~ created ~ updated ~ transfers => 
        Topic(id, scheme_id, topicId, title, created, updated, transfers)
    }
  }

  def all: List[Topic] = {
      DB.withConnection { implicit c =>
      SQL("select * from topic").as(topic *)
    }
  }

  def count = {
     DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from topic").apply.head
      count[Long]("c")
    }
  }

  def withScheme(scheme_id: Long): List[Topic] = {
      DB.withConnection { implicit c =>
      SQL("select * from topic where scheme_id = {scheme_id}").on('scheme_id -> scheme_id).as(topic *)
    }   
  }

  def findById(id: Long): Option[Topic] = {
   DB.withConnection { implicit c =>
      SQL("select * from topic where id = {id}").on('id -> id).as(topic.singleOpt)
    }  
  }

  def create(scheme_id: Long, topicId: String, title: String) {
    val created = new Date
    val updated = created
		DB.withConnection { implicit c =>
			SQL("insert into topic (scheme_id, topicId, title, created, updated, transfers) values ({scheme_id}, {topicId}, {title}, {created}, {updated}, {transfers})")
      .on('scheme_id -> scheme_id, 'topicId -> topicId, 'title -> title, 'created -> created, 'updated -> updated, 'transfers -> 0).executeUpdate()
		}
  }

  def forSchemeAndId(schemeId: String, topicId: String): Option[Topic] = {
   DB.withConnection { implicit c =>
      SQL("select topic.* from scheme, topic where scheme.id = topic.scheme_id and scheme.schemeId = {schemeId} and topic.topicId = {topicId}")
      .on('schemeId -> schemeId, 'topicId -> topicId).as(topic.singleOpt)
    }
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from topic where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
