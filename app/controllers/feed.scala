/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package controllers

import scala.xml.{NodeSeq, XML}

import org.joda.time.format.ISODateTimeFormat

import play.api._
import play.api.libs.iteratee.Enumerator
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current

import models.Topic

object Feed extends Controller {

  val hubNs = "http://topichub"
  val iso8601 = ISODateTimeFormat.dateTime

  /**
   * Generates a feed for topic identified by scheme+topicId
   * @return Atom document or 404 if no topic exists
   */
  def topicFeed(scheme: String, topicId: String) = Action { implicit request =>
    Topic.forSchemeAndId(scheme, topicId).map ( topic =>
      SimpleResult(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> "application/atom+xml; charset=\"utf-8\"")),
        body = Enumerator(atomFeed(topic, request.host, request.uri))
      )
    ).getOrElse(NotFound("No such topic " + scheme + ":" + topicId))
  }

  /**
   * Creates an Atom feed document for passed topic
   * @return XML NodeSeq representing feed
   */
  private def atomFeed(topic: Topic, host: String, uri: String) =
    <feed xmlns="http://www.w3.org/2005/Atom">
      <title>{topic.title}</title>
      <link href={"http://" + host}/>
      <link rel="self" href={"http://" + host + uri} />
      <updated>{iso8601.print(topic.created.getTime())}</updated>
      <id>{hubNs + uri}</id>
      <generator uri={hubNs + "/ns/sword/1.3"} version="1.0">Topic Hub</generator>
      { for (item <- topic.items) yield
        <entry>
          <title>{item.metadataValue("title")}</title>
          <link href={"http://" + host + "/item/" + item.id}/>
          <link rel="sword" href={"http://" + host + "/sword/request/" + item.itemId}/>
          <id>md5:{item.itemId}</id>
          <updated>{iso8601.print(item.created.getTime())}</updated>
          { for (auth <- item.metadataValues("authors")) yield
            <author>
              <name>{auth}</name>
            </author>
          }
          <summary>
            {item.metadataValue("title")} {item.metadataValues("authors").mkString(",")} {topic.title}
          </summary>
        </entry>
      }
    </feed>
}
