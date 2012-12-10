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
  * Subscription is an association between a subscriber channel and a topic,
  * the fulfillment of which is a transfer of one or more items or notifications
  * through the channel
  *
  * @author richardrodgers
  */

case class Subscription(id: Long, subscriber_id: Long, channel_id: Long, topic_id: Long,
                        policy: String = "all", created: Date, updated: Date, transfers: Int) {

  def recordTransfer {
    val newTrans = transfers + 1
    DB.withConnection { implicit c =>
      SQL("update subscription set transfers = {transfers}, updated = {updated} where id = {id} ")
      .on('transfers -> newTrans, 'updated -> new Date, 'id -> id).executeUpdate()
    }
  }
}

object Subscription {

  val subscription = {
    get[Long]("id") ~ get[Long]("subscriber_id") ~ get[Long]("channel_id") ~ get[Long]("topic_id") ~
    get[String]("policy") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~ subscriber_id ~ channel_id ~ topic_id ~ policy ~ created ~ updated ~ transfers => 
        Subscription(id, subscriber_id, channel_id, topic_id, policy, created, updated, transfers)
    }
  }

  def findById(id: Long): Option[Subscription] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscription where id = {id}").on('id -> id).as(subscription.singleOpt)
    }
  }

  def findByTopicAndChannel(topic_id: Long, channel_id: Long): List[Subscription] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscription where topic_id = {topic_id} and channel_id = {channel_id}")
      .on('topic_id -> topic_id, 'channel_id -> channel_id).as(subscription *)
    }
  }

  def create(subscriber_id: Long, channel_id: Long, topic_id: Long, policy: String) {
		DB.withConnection { implicit c =>
			SQL("insert into subscription (subscriber_id, channel_id, topic_id, policy, created, updated, transfers) values ({subscriber_id}, {channel_id}, {topic_id}, {policy}, {created}, {updated}, {transfers})")
      .on('subscriber_id -> subscriber_id, 'channel_id -> channel_id, 'topic_id -> topic_id, 'policy -> policy, 'created -> new Date, 'updated -> new Date, 'transfers -> 0).executeUpdate()
		}
  }

  def make(subscriber_id: Long, channel_id: Long, topic_id: Long, policy: String): Subscription = {
    create(subscriber_id, channel_id, topic_id, policy)
    findByTopicAndChannel(topic_id, channel_id).head
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from subscription where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}