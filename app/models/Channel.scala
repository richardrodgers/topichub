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

/** Channel contains coordinates and credentials to effect a content transfer
  * or notification between a hub and a specified address or endpoint of a subscriber
  * or publisher.
  *
  * @author richardrodgers
  */

case class Channel(id: Long, protocol: String, mode: String, direction: String, description: String,
                  userId: String, password: String, channelUrl: String, created: Date, updated: Date, transfers: Int) {

  def subscriptions = {
   DB.withConnection { implicit c =>
      SQL("select * from subscription where channel_id = {id}")
      .on('id -> id).as(Subscription.subscription *)
    }
  }

  def recordTransfer {
    val newTrans = transfers + 1
    DB.withConnection { implicit c =>
      SQL("update channel set transfers = {transfers}, updated = {updated} where id = {id} ")
      .on('transfers -> newTrans, 'updated -> new Date, 'id -> id).executeUpdate()
    }
  }

  def setOwner(ownerType: String, owner_id: Long) {
    DB.withConnection { implicit c =>
      SQL("insert into channelowner (channel_id, owner_type, owner_id) values ({channel_id}, {ownerType}, {owner_id})")
      .on('channel_id -> id, 'ownerType -> ownerType, 'owner_id -> owner_id).executeInsert()
    }
  }
}

object Channel {

  val channel = {
    get[Long]("id") ~ get[String]("protocol") ~ get[String]("mode") ~ get[String]("direction") ~ 
    get[String]("description") ~ get[String]("user_id") ~ get[String]("password") ~
    get[String]("channel_url") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~  protocol ~ mode ~ direction ~ description ~ userId ~ password ~ channelUrl ~ created ~ updated ~ transfers => 
        Channel(id, protocol, mode, direction, description, userId, password, channelUrl, created, updated, transfers)
    }
  }

  def findById(id: Long): Option[Channel] = {
    DB.withConnection { implicit c =>
      SQL("select * from channel where id = {id}").on('id -> id).as(channel.singleOpt)
    }
  }

  def create(protocol: String, mode: String, direction: String, description: String, userId: String, password: String, channelUrl: String) = {
		DB.withConnection { implicit c =>
			SQL("insert into channel (protocol, mode, direction, description, user_id, password, channel_url, created, updated, transfers) values ({protocol}, {mode}, {direction}, {description}, {userId}, {password}, {channelUrl}, {created}, {updated}, {transfers})")
      .on('protocol -> protocol, 'mode -> mode, 'direction -> direction, 'description -> description, 'userId -> userId, 'password -> password, 'channelUrl -> channelUrl, 'created -> new Date, 'updated -> new Date, 'transfers -> 0).executeInsert()
		}
  }

  def make(protocol: String, mode: String, direction: String, description: String, userId: String, password: String, channelUrl: String) = {
    findById(create(protocol, mode, direction, description, userId, password, channelUrl).get).get
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from target where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
