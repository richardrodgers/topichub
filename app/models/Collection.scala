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

/** Collection is a content aggregation where elements share a content type and packaging type.
  * Collections belong to Publishers
  *
  * @author richardrodgers
  */

case class Collection(id: Long, publisher_id: Long, ctype_id: Long, pkgmap_id: Long, description: String,
                      policy: String, created: Date, updated: Date, deposits: Int) {

  def packaging = {
    DB.withConnection { implicit c =>
      val pkg = SQL("select pkgmap.swordurl from pkgmap, collection where collection.pkgmap_id = pkgmap.id and collection.id = {id}")
               .on('id -> id).apply().head
      pkg[String]("swordurl")
    } 
  }

  def recordDeposit {
    val newDep = deposits + 1
    DB.withConnection { implicit c =>
      SQL("update collection set deposits = {deposits} where id = {id} ")
      .on('deposits -> newDep, 'id -> id).executeUpdate()
    }
  }
}

object Collection {

  val coll = {
    get[Long]("id") ~ get[Long]("publisher_id") ~ get[Long]("ctype_id")  ~ get[Long]("pkgmap_id") ~ get[String]("description") ~ 
    get[String]("policy") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("deposits") map {
      case id ~ publisher_id ~ ctype_id ~ pkgmap_id ~ description ~ policy ~ created  ~ updated ~ deposits => 
        Collection(id, publisher_id, ctype_id, pkgmap_id, description, policy, created, updated, deposits)
    }
  }

  def findAll: List[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection").as(coll *)
    }  
  }

  def findById(id: Long): Option[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection where id = {id}").on('id -> id).as(coll.singleOpt)
    }
  }

  def findByCollId(collId: String): Option[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection where collId = {collId}").on('collId -> collId).as(coll.singleOpt)
    }
  }

  def findByPublisher(pubId: Long): List[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection where publisher_id = {pubId}").on('pubId -> pubId).as(coll *)
    }
  }

  def create(publisher_id: Long, ctype_id: Long, pkgmap_id: Long, description: String, policy: String) {
    val created = new Date
    val updated = created
		DB.withConnection { implicit c =>
			SQL("insert into collection (publisher_id, ctype_id, pkgmap_id, description, policy, created, updated, deposits) values ({publisher_id}, {ctype_id}, {pkgmap_id}, {description}, {policy}, {created}, {updated}, {deposits})")
      .on('publisher_id -> publisher_id, 'ctype_id -> ctype_id, 'pkgmap_id -> pkgmap_id, 'description -> description, 'policy -> policy, 'created -> created, 'updated -> updated, 'deposits -> 0).executeUpdate()
		}
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from collection where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
