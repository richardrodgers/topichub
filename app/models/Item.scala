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
import anorm.SimpleSql
import anorm.Row

case class Item(id: Long, collection_id: Long, ctype_id: Long, itemId: String,
                created: Date, updated: Date, transfers: Int) {

  def addTopic(topic: Topic) {
    DB.withConnection { implicit c =>
      SQL("insert into itemtopic (item_id, topic_id) values ({item_id}, {topic_id})")
      .on('item_id -> id, 'topic_id -> topic.id).executeUpdate()
    }
  } 

  def topics = {
    DB.withConnection { implicit c =>
      SQL("select topic.* from topic, itemtopic where topic.id = itemtopic.topic_id and itemtopic.item_id = {item_id}")
      .on('item_id -> id).as(Topic.topic *)
    }
  }

  def addMetadata(mdname: String, mdvalue: String) {
    DB.withConnection { implicit c =>
      SQL("insert into metadata (item_id, mdname, mdvalue) values ({item_id}, {mdname}, {mdvalue})")
      .on('item_id -> id, 'mdname -> mdname, 'mdvalue -> mdvalue).executeUpdate()
    }   
  }

  def metadataValue(mdname: String) = {
    DB.withConnection { implicit c =>
      SQL("select mdvalue from metadata where item_id = {item_id} and mdname = {mdname}")
      .on('item_id -> id, 'mdname -> mdname).apply().headOption match {
        case Some(x) => x[String]("mdvalue")
        case None => "Unknown Value"
      }
    }
  }

  def metadataValues(mdname: String) = {
    DB.withConnection { implicit c =>
      val rows = SQL("select mdvalue from metadata where item_id = {item_id} and mdname = {mdname}")
      .on('item_id -> id, 'mdname -> mdname)
      rows().map(row => row[String]("mdvalue")).toList
    }
  }

  def recordTransfer {
    val newTrans = transfers + 1
    DB.withConnection { implicit c =>
      SQL("update item set transfers = {transfers}, updated = {updated} where id = {id} ")
      .on('transfers -> newTrans, 'updated -> new Date, 'id -> id).executeUpdate()
    }
  }
}

object Item {

  val item = {
    get[Long]("id") ~ get[Long]("collection_id") ~ get[Long]("ctype_id") ~ get[String]("itemId") ~
    get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~ collection_id ~ ctype_id ~ itemId ~ created ~ updated ~ transfers => 
        Item(id, collection_id, ctype_id, itemId, created, updated, transfers)
    }
  }

  def create(collection_id: Long, ctype_id: Long, itemId: String) {
		DB.withConnection { implicit c =>
			SQL("insert into item (collection_id, ctype_id, itemId, created, updated, transfers) values ({collection_id}, {ctype_id}, {itemId}, {created}, {updated}, {transfers})")
      .on('collection_id -> collection_id, 'ctype_id -> ctype_id, 'itemId -> itemId, 'created -> new Date, 'updated -> new Date, 'transfers -> 0).executeUpdate()
		}
  }

  def all: List[Item] = {
      DB.withConnection { implicit c =>
        SQL("select * from item").as(item *)
    }
  }

  def count = {
     DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from item").apply.head
      count[Long]("c")
    }
  }

  def findByRange(fromO: Option[String], tillO: Option[String]): List[Item] = {
    val from: String = null
    val till: String = null
    DB.withConnection { implicit c => {
      var hits: SimpleSql[Row] = null
      if (from != null && till != null) {
        hits = SQL("select * from item where created >= {from} and created <= {till}").on('from -> from, 'till -> till)
      } else if (from != null) {
        hits = SQL("select * from item where created >= {from}").on('from -> from)
      } else if (till != null) {
        hits = SQL("select * from itme where created <= {till}").on('till -> till)
      } else {
        hits = SQL("select * from item")
      }
      hits.as(item *)
    }}
  }

  def findById(id: Long): Option[Item] = {
    DB.withConnection { implicit c =>
      SQL("select * from item where id = {id}").on('id -> id).as(item.singleOpt)
    }  
  }

  def findOldest: Option[Item] = {
     DB.withConnection { implicit c =>
      SQL("select * from item order by created asc").as(item.singleOpt)
    }
  }

  def findByItemId(itemId: String): Option[Item] = {
   DB.withConnection { implicit c =>
      SQL("select * from item where itemId = {itemId}").on('itemId -> itemId).as(item.singleOpt)
    }
  }

  def inCollection(coll_id: Long): List[Item] = {
    DB.withConnection { implicit c =>
      SQL("select * from item where collection_id = {id}").on('id -> coll_id).as(item *)
    }  

  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
      SQL("delete from metadata where item_id = {item_id}").on('item_id -> id).executeUpdate()
  		SQL("delete from item where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
