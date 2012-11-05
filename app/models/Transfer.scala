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

/** Transfer is a content delivery to a destination, either a subscription target or a simple
  * download.
  *
  * @author richardrodgers
  */

case class Transfer(id: Long, target_id: Long, item_id: Long, subscription_id: Long, target_addr: Option[String],
                    created: Date, state: String, modified: Date) {

  def updateState(state: String) {
    DB.withConnection { implicit c =>
      SQL("update transfer set state = {state} and modified = {modified} where id = {id} ")
      .on('state -> state, 'modified -> new Date, id -> id).executeUpdate()
    }
  }
}

object Transfer {

  val transfer = {
    get[Long]("id") ~ get[Long]("target_id") ~ get[Long]("item_id")  ~ get[Long]("subscription_id") ~ get[String]("target_addr") ~ 
    get[Date]("created") ~ get[String]("state") ~ get[Date]("modified") map {
      case id ~ target_id ~ item_id ~ subscription_id ~ target_addr ~ created ~ state ~ modified => 
        Transfer(id, target_id, item_id, subscription_id, Some(target_addr), created, state, modified)
    }
  }

  def create(target_id: Long, item_id: Long, subscription_id: Long, target_addr: Option[String]) {
		DB.withConnection { implicit c =>
			SQL("insert into transfer (target_id, item_id, subscription_id, target_addr, created, state, modified) values ({target_id}, {item_id}, {subscription_id}, {target_addr}, {created}, {state}, {modified})")
      .on('target_id -> target_id, 'item_id -> item_id, 'subscription_id -> subscription_id, 'target_addr -> target_addr, 'created -> new Date, 'state -> "new", 'modified -> new Date).executeUpdate()
		}
  }

  def findByTargetAndItem(target_id: Long, item_id: Long): List[Transfer] = {
    DB.withConnection { implicit c =>
      SQL("select * from transfer where target_id = {target_id} and item_id = {item_id}").on('target_id -> target_id, 'item_id -> item_id).as(transfer *)
    }
  }

  def make(target_id: Long, item_id: Long, subscription_id: Long, target_addr: Option[String]): Transfer = {
    create(target_id, item_id, subscription_id, target_addr)
    findByTargetAndItem(target_id, item_id).head
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from transfer where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}