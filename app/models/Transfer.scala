/**
 * Copyright 2012 MIT Libraries
 * Licensed under:  http://www.apache.org/licenses/LICENSE-2.0
 */
package models

import java.util.Date

import play.api.db.DB
import play.api.Play.current

import anorm.SQL

/**
  * Transfer is a content delivery to a destination, either a subscription target or a simple
  * download.
  *
  * @author richardrodgers
  */

case class Transfer(id: Long, target_id: Long, subscription_id: Long, itemId: String, target_addr: Option[String],
                    created: Date, state: String, modified: Date)

object Transfer {

  def create(target_id: Long, subscription_id: Long, itemId: String, target_addr: Option[String]) {
		DB.withConnection { implicit c =>
			SQL("insert into transfer (target_id, subscription_id, itemId, target_addr, created, state, modified) values ({target_id}, {subscription_id}, {itemId}, {target_addr}, {created}, {state}, {modified})")
      .on('target_id -> target_id, 'subscription_id -> subscription_id, 'itemId -> itemId, 'target_addr -> target_addr, 'created -> new Date, 'state -> "new", 'modified -> new Date).executeUpdate()
		}
  }
  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from transfer where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}