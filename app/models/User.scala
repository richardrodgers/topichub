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
import anorm.SqlRow

/** User is an authenticated identity to a hub, who may be a publisher, subscriber, editor,
  * or site adminsirator.
  *
  * @author richardrodgers
  */

case class User(id: Long, name: String, email: String, password: String,
                role: String, created: Date, accessed: Date) {

  def recordVisit {
    DB.withConnection { implicit c =>
      SQL("update hubuser set accessed = {accessed} where id = {id}")
      .on('accessed -> new Date, 'id -> id).executeUpdate()
    }
  }
}

object User {

  val user = {
    get[Long]("id") ~ get[String]("name") ~ get[String]("email") ~ get[String]("password") ~
    get[String]("role") ~ get[Date]("created") ~ get[Date]("accessed") map {
      case id ~ name ~ email ~ password ~ role ~ created ~ accessed => 
        User(id, name, email, password, role, created, accessed)
    }
  }

  def findById(id: Long): Option[User] = {
    DB.withConnection { implicit c =>
      SQL("select * from hubuser where id = {id}").on('id -> id).as(user.singleOpt)
    }
  }

  def findByName(name: String): Option[User] = {
    DB.withConnection { implicit c =>
      SQL("select * from hubuser where name = {name}").on('name -> name).as(user.singleOpt)
    }
  }

  def all: List[User] = {
    DB.withConnection { implicit c =>
      SQL("select * from hubuser").as(user *)
    }
  }

  def create(name: String, email: String, password: String, role: String) {
    DB.withConnection { implicit c =>
      SQL("insert into hubuser (name, email, password, role, created, accessed) values ({name}, {email}, {password}, {role}, {created}, {accessed})")
      .on('name -> name, 'email -> email, 'password -> password, 'role -> role, 'created -> new Date, 'accessed -> new Date).executeUpdate()
    }
  }

  def authenticate(username: String, password: String): Option[User] = {
    DB.withConnection { implicit c =>
      SQL("select * from hubuser where name = {username} and password = {password}").on('username -> username, 'password -> password).as(user.singleOpt)
    }
  }

  def delete(id: Long) {
    DB.withConnection { implicit c =>
      SQL("delete from hubuser where id = {id}").on('id -> id).executeUpdate()
    }
  }
}

