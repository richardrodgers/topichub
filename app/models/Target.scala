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

/** Target contains coordinates and credentials to effect a content transfer
  * from hub to a specified address or endpoint of a subscriber.
  *
  * @author richardrodgers
  */

case class Target(id: Long, subscriber_id: Long, description: String, userId: String, password: String,
                  depositUrl: String, created: Date, updated: Date, transfers: Int) {

  def subscriptions = {
   DB.withConnection { implicit c =>
      SQL("select * from subscription where target_id = {id}")
      .on('id -> id).as(Subscription.subscription *)
    }
  }

  def recordTransfer {
    val newTrans = transfers + 1
    DB.withConnection { implicit c =>
      SQL("update target set transfers = {transfers} and updated = {updated} where id = {id} ")
      .on('transfers -> newTrans, 'updated -> new Date, id -> id).executeUpdate()
    }
  }
}

object Target {

  val target = {
    get[Long]("id") ~ get[Long]("subscriber_id") ~ get[String]("description") ~ get[String]("userId") ~ get[String]("password") ~
    get[String]("depositUrl") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~ subscriber_id ~ description ~ userId ~ password ~ depositUrl ~ created ~ updated ~ transfers => 
        Target(id, subscriber_id, description, userId, password, depositUrl, created, updated, transfers)
    }
  }

  def findById(id: Long): Option[Target] = {
    DB.withConnection { implicit c =>
      SQL("select * from target where id = {id}").on('id -> id).as(target.singleOpt)
    }
  }

  def create(subscriber_id: Long, description: String, userId: String, password: String, depositUrl: String) {
		DB.withConnection { implicit c =>
			SQL("insert into target (subscriber_id, description, userId, password, depositUrl, created, updated, transfers) values ({subscriber_id}, {description}, {userId}, {password}, {depositUrl}, {created}, {updated}, {transfers})")
      .on('subscriber_id -> subscriber_id, 'description -> description, 'userId -> userId, 'password -> password, 'depositUrl -> depositUrl, 'created -> new Date, 'updated -> new Date, 'transfers -> 0).executeUpdate()
		}
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from target where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
