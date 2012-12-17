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

/** Transfer is a content delivery to a destination, either via a subscription target or a simple
  * ad-hoc request.
  *
  * @author richardrodgers
  */

case class Transfer(id: Long, channel_id: Long, item_id: Long, subscription_id: Long, transfer_addr: Option[String],
                    created: Date, state: String, modified: Date) {

  def updateState(state: String) {
    DB.withConnection { implicit c =>
      SQL("update transfer set state = {state}, modified = {modified} where id = {id} ")
      .on('state -> state, 'modified -> new Date, 'id -> id).executeUpdate()
    }
  }

  def channel = {
    DB.withConnection { implicit c =>
      SQL("select * from channel where id = {channel_id}").on('channel_id -> channel_id).as(Channel.channel.singleOpt)
    }
  }
}

object Transfer {

  val transfer = {
    get[Long]("id") ~ get[Long]("channel_id") ~ get[Long]("item_id") ~ get[Long]("subscription_id") ~ get[Option[String]]("transfer_addr") ~ 
    get[Date]("created") ~ get[String]("state") ~ get[Date]("modified") map {
      case id ~ channel_id ~ item_id ~ subscription_id ~ transfer_addr ~ created ~ state ~ modified => 
        Transfer(id, channel_id, item_id, subscription_id, transfer_addr, created, state, modified)
    }
  }

  def findById(id: Long): Option[Transfer] = {
    DB.withConnection { implicit c =>
      SQL("select * from transfer where id = {id}").on('id -> id).as(transfer.singleOpt)
    }    
  }

  def create(channel_id: Long, item_id: Long, subscription_id: Long, transfer_addr: Option[String]) = {
		DB.withConnection { implicit c =>
			SQL("insert into transfer (channel_id, item_id, subscription_id, transfer_addr, created, state, modified) values ({channel_id}, {item_id}, {subscription_id}, {transfer_addr}, {created}, {state}, {modified})")
      .on('channel_id -> channel_id, 'item_id -> item_id, 'subscription_id -> subscription_id, 'transfer_addr -> transfer_addr, 'created -> new Date, 'state -> "new", 'modified -> new Date).executeInsert()
		}
  }

  def findByChannelAndItem(channel_id: Long, item_id: Long): List[Transfer] = {
    DB.withConnection { implicit c =>
      SQL("select * from transfer where channel_id = {channel_id} and item_id = {item_id}").on('channel_id -> channel_id, 'item_id -> item_id).as(transfer *)
    }
  }

  def make(channel_id: Long, item_id: Long, subscription_id: Long, transfer_addr: Option[String]): Transfer = {
    findById(create(channel_id, item_id, subscription_id, transfer_addr).get).get
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from transfer where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}