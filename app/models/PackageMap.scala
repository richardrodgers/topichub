/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package models

import play.api.db.DB
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL

/** PackageMap provides an information map of a package containing item content.
  * It locates scheme resources via package-relative file names.
  *
  * @author richardrodgers
  */

case class PackageMap(id: Long, pkgmapId: String, description: String, swordUrl: Option[String]) {

  def addMapping(scheme_id: Long, source: String, format: String, rank: Int) {
    DB.withConnection { implicit c =>
      SQL("insert into pkgmapscheme (pkgmap_id, scheme_id, source, format, rank) values ({pkgmap_id}, {scheme_id}, {source}, {format}, {rank})")
      .on('pkgmap_id -> id, 'scheme_id -> scheme_id, 'source -> source, 'format -> format, 'rank -> rank).executeUpdate()
    }
  }

  def mappingsForScheme(scheme: Scheme): List[(String, String, Int)] = {
     DB.withConnection { implicit c =>
      SQL("select source,format,rank from pkgmapscheme where scheme_id = {scheme_id} and pkgmap_id = {pkgmap_id}")
      .on('scheme_id -> scheme.id, 'pkgmap_id -> id).as(str("source") ~ str("format") ~ int("rank") map(flatten) *)
    }
  }

  def schemes: List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select scheme.* from scheme, pkgmapscheme, pkgmap where scheme.id = pkgmapscheme.scheme_id and pkgmapscheme.pkgmap_id = pkgmap.id and pkgmap.id = {pkgmap_id}")
      .on('pkgmap_id -> id).as(Scheme.scheme *)
    }
  }

  def removeMapping(scheme: Scheme, source: String) {
    DB.withConnection { implicit c =>
      SQL("delete from pkgmapscheme where pkgmap_id = {pkgmap_id} and scheme_id = {scheme_id} and source = {source}")
      .on('pkgmap_id -> id, 'scheme_id -> scheme.id, 'source -> source).executeUpdate()
    }  
  }
}

object PackageMap {

  val pkgmap = {
    get[Long]("id") ~ get[String]("pkgmap_id") ~ get[String]("description") ~ get[String]("sword_url") map {
      case id ~ pkgmapId ~ description ~ swordUrl => PackageMap(id, pkgmapId, description, Some(swordUrl))
    }
  }

  def create(pkgmapId: String, description: String, swordUrl: Option[String]) {
		DB.withConnection { implicit c =>
			SQL("insert into pkgmap (pkgmap_id, description, sword_url) values ({pkgmapId}, {description}, {swordUrl})")
      .on('pkgmapId -> pkgmapId, 'description -> description, 'swordUrl  -> swordUrl).executeUpdate()
		}
  }

  def all: List[PackageMap] = {
    DB.withConnection { implicit c =>
      SQL("select * from pkgmap").as(pkgmap *)
    }   
  }

  def findById(id: Long): Option[PackageMap] = {
    DB.withConnection { implicit c =>
      SQL("select * from pkgmap where id = {id}").on('id -> id).as(pkgmap.singleOpt)
    }
  }

  def findByName(pkgmapId: String): Option[PackageMap] = {
      DB.withConnection { implicit c =>
      SQL("select * from pkgmap where pkgmap_id = {pkgmapId}").on('pkgmapId -> pkgmapId).as(pkgmap.singleOpt)
    }  
  }

  def delete(id: Long) {
  	DB.withConnection { implicit c =>
      SQL("delete from pkgmapscheme where pkgmap_id = {id}").on('id -> id).executeUpdate()
  		SQL("delete from pkgmap where id = {id}").on('id -> id).executeUpdate()
  	}
  }

  def mapView: Map[String, String] = {
      all map (pm => pm.id.toString -> pm.pkgmapId) toMap
  }
}
