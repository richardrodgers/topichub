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
  * Subscriber is an entity that receives notificatons and/or content deliveries based on hub-registered intents
  * known as subscriptions.
  *
  * @author richardrodgers
  */

case class Subscriber(id: Long, user_id: Long, category: String, name: String, visibility: String, keywords: String,
                      link: Option[String], logo: Option[String], contact: String, swordService: String,
                      terms: String, backFile: String, created: Date) {

  def channels = {
    DB.withConnection { implicit c =>
      SQL("select channel.* from channel, channelowner where channel.id = channelowner.channel_id and channelowner.owner_type = 'sub' and channelowner.owner_id = {id}")
      .on('id -> id).as(Channel.channel *)
    }
  }

  def channelsWith(mode: String) = {
    DB.withConnection { implicit c =>
      SQL("select channel.* from channel, channelowner where channel.id = channelowner.channel_id and channelowner.owner_type = 'sub' and channelowner.owner_id = {id} and channel.mode = {mode}")
      .on('id -> id, 'mode -> mode).as(Channel.channel *)
    }
  }

  def subscriptionCount = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from subscription where subscriber_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def recentTransfers(max: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select transfer.* from transfer, channelowner
          where transfer.channel_id = channelowner.channel_id and owner_type = 'sub' and owner_id = {id}
          order by transfer.created desc limit {max}
        """
      ).on('id -> id, 'max -> max).as(Transfer.transfer *)
    }
  }
}

object Subscriber {

  val subscriber = {
    get[Long]("id") ~ get[Long]("user_id") ~ get[String]("category") ~ get[String]("name") ~ get[String]("visibility") ~
    get[String]("keywords") ~  get[String]("link") ~ get[String]("logo") ~ get[String]("contact") ~ get[String]("sword_service") ~ 
    get[String]("terms") ~ get[String]("back_file") ~ get[Date]("created") map {
      case id ~ user_id ~ category ~ name ~ visibility ~ keywords ~ link ~ logo ~ contact ~ swordService ~ terms ~ backFile ~ created => 
      Subscriber(id, user_id, category, name, visibility, keywords, Some(link), Some(logo), contact, swordService, terms, backFile, created)
    }
  }

  def all: List[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber").as(subscriber *)
    }   
  }

  def count = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from subscriber").apply.head
      count[Long]("c")
    }
  }

  def categories = {
    DB.withConnection { implicit c =>
      SQL("select distinct category from subscriber").as(scalar[String] *)
    }
  }

  def categoryCount(category: String) = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from subscriber where category = {category}").on('category -> category).apply.head
      count[Long]("c")
    }
  }

  def inCategory(category: String, page: Int) = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from subscriber where category = {category}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('category -> category, 'offset -> offset).as(subscriber *)
    }  
  }

  def findById(id: Long): Option[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where id = {id}").on('id -> id).as(subscriber.singleOpt)
    }
  }

  def findByUserId(user_id: Long): Option[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where user_id = {user_id}").on('user_id -> user_id).as(subscriber.singleOpt)
    }
  }

  def findByContact(contact: String): Option[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where contact = {contact}").on('contact -> contact).as(subscriber.singleOpt)
    }
  }

  def create(user_id: Long, category: String, name: String, visibility: String, keywords: String, link: Option[String],
             logo: Option[String], contact: String, swordService: String, terms: String, backFile: String) {
		DB.withConnection { implicit c =>
			SQL("insert into subscriber (user_id, category, name, visibility, keywords, link, logo, contact, sword_service, terms, back_file, created) values ({user_id}, {category}, {name}, {visibility}, {keywords}, {link}, {logo}, {contact}, {swordService}, {terms}, {backFile}, {created})")
      .on('user_id -> user_id, 'category -> category, 'name -> name, 'visibility -> visibility, 'keywords -> keywords, 'link -> link, 'logo -> logo, 'contact -> contact, 'swordService -> swordService, 'terms -> terms, 'backFile -> backFile, 'created -> new Date).executeUpdate()
		}
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
  		SQL("delete from subscriber where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}