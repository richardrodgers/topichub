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

/** Topic represents a value in a namespace (its scheme). Topics are assigned
  * to items, and become the object of subscriptions.
  *
  * @author richardrodgers
  */

case class Topic(id: Long, scheme_id: Long, topicId: String, name: String,
                 link: Option[String], created: Date, updated: Date, transfers: Int) {

  def items = {
   DB.withConnection { implicit c =>
      SQL("select item.* from item, itemtopic where item.id = itemtopic.item_id and itemtopic.topic_id = {topic_id}")
      .on('topic_id -> id).as(Item.item *)
    }
  }

  def recentItems(max: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select item.* from item, itemtopic
          where item.id = itemtopic.item_id and itemtopic.topic_id = {topic_id}
          order by item.created desc limit {max}
        """
      ).on('topic_id -> id, 'max -> max).as(Item.item *)
    }
  }

  def pagedItems(page: Int, max: Int) = {
    val offset = page * max
    DB.withConnection { implicit c =>
      SQL(
        """
          select item.* from item, itemtopic
          where item.id = itemtopic.item_id and itemtopic.topic_id = {topic_id}
          order by item.created desc
          limit {max} offset {offset}
        """
      ).on('topic_id -> id, 'max -> max, 'offset -> offset).as(Item.item *)
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

  def subscriptions = {
    DB.withConnection { implicit c =>
      SQL("select * from subscription where topic_id = {id}").on('id -> id).as(Subscription.subscription *)
    }    
  }

  def recordTransfer {
    val newTrans = transfers + 1
    DB.withConnection { implicit c =>
      SQL("update topic set transfers = {transfers}, updated = {updated} where id = {id} ")
      .on('transfers -> newTrans, 'updated -> new Date, 'id -> id).executeUpdate()
    }
  }

  def setSetter(setterType: String, setter_id: Long) {
    DB.withConnection { implicit c =>
      SQL("insert into topicsetter (topic_id, setter_type, setter_id) values ({topic_id}, {setterType}, {setter_id})")
      .on('topic_id -> id, 'setterType -> setterType, 'setter_id -> setter_id).executeInsert()
    }
  }

}

object Topic {

  val topic = {
    get[Long]("id") ~ get[Long]("scheme_id") ~ get[String]("topic_id") ~ get[String]("name") ~ 
    get[Option[String]]("link") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~ scheme_id ~ topicId ~ name ~ link ~ created ~ updated ~ transfers => 
        Topic(id, scheme_id, topicId, name, link, created, updated, transfers)
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

  def withScheme(scheme_id: Long, page: Int): List[Topic] = {
      val offset = page * 10
      DB.withConnection { implicit c =>
      SQL(
        """
          select * from topic
          where scheme_id = {scheme_id}
          order by name
          limit 10 offset {offset}
        """
      ).on('scheme_id -> scheme_id, 'offset -> offset).as(topic *)
    }   
  }

  def findById(id: Long): Option[Topic] = {
   DB.withConnection { implicit c =>
      SQL("select * from topic where id = {id}").on('id -> id).as(topic.singleOpt)
    }  
  }

  def create(scheme_id: Long, topicId: String, name: String) = {
    val created = new Date
    val updated = created
		DB.withConnection { implicit c =>
			SQL(
        """
          insert into topic (scheme_id, topic_id, name, created, updated, transfers)
          values ({scheme_id}, {topicId}, {name}, {created}, {updated}, {transfers})
        """
      ).on('scheme_id -> scheme_id, 'topicId -> topicId, 'name -> name, 'created -> created, 'updated -> updated, 'transfers -> 0).executeInsert()
		}
  }

  def make(scheme_id: Long, topicId: String, name: String): Topic = {
    findById(create(scheme_id, topicId, name).get).get
  }

  def forSchemeAndId(schemeId: String, topicId: String): Option[Topic] = {
   DB.withConnection { implicit c =>
      SQL("select topic.* from scheme, topic where scheme.id = topic.scheme_id and scheme.scheme_id = {schemeId} and topic.topic_id = {topicId}")
      .on('schemeId -> schemeId, 'topicId -> topicId).as(topic.singleOpt)
    }
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from topic where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
