/**
  * Copyright 2012 MIT Libraries
  * Licensed under:  http://www.apache.org/licenses/LICENSE-2.0
  */
package controllers

import scala.xml.NodeSeq

import java.util.Date

import org.joda.time.DateTimeZone
import org.joda.time.format.ISODateTimeFormat

import play.api.libs.iteratee.Enumerator
import play.api.mvc._

import models.{Item, Scheme, Topic}

/**
  * OAI object manages OAI-PMH (Open Archives Initiative - Protocol for Metadata
  * Harvesting) v2.0 protocol operations. The hub is a 'repository' (data 
  * provider) in OAI-PMH terms, meaning it responds in the protocol-ordained
  * manner to 'harvesters' requests. The data model:
  * OAI Items = hub Items, and OAI Sets = hub Topics.
  * Features to be implemented: Resumption Tokens, real error condition checking
  *
  * @author richardrodgers
  */

object OAI extends Controller {

  val iso8601 = ISODateTimeFormat.dateTimeNoMillis.withZone(DateTimeZone.UTC)
  lazy val metaSchemeId = Scheme.findByName("meta") match { case Some(s) => s.id; case _ => 0L }
  lazy val earliestDatestamp = Item.findOldest match { case Some(i) => i.created.getTime(); case _ => (new Date).getTime }

  def provide = Action { implicit request =>
    val query = request.queryString.mapValues(_.head)
    SimpleResult(
      header = ResponseHeader(200, Map(CONTENT_TYPE -> "text/xml; charset=\"utf-8\"")),
      body = Enumerator(
        <OAI-PMH xmlns="http://www.openarchives.org/OAI/2.0/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.openarchives.org/OAI/2.0/
           http://www.openarchives.org/OAI/2.0/OAI-PMH.xsd">
          <responseDate>{iso8601.print(System.currentTimeMillis())}</responseDate>
          { reqAttrs(query, request.host, request.path) }
          { if (query.size > 0) processQuery(query, request.host, request.path) else error("badVerb", "Missing verb") }
        </OAI-PMH>)
      )
  }

  private def reqAttrs(query: Map[String, String], host: String, path: String) = {
    var attrs: StringBuilder = new StringBuilder
    for (arg <- query.keysIterator) {
      attrs.append(arg).append("=\"").append(query.get(arg).get).append("\" ")
    }
    "<request " + attrs.toString.trim + ">" + "http://" + host + path + "</request>"
  }

  private def processQuery(query: Map[String, String], host: String, path: String): NodeSeq = {
    val verb = query.get("verb").get
    verb match {
      case "GetRecord" => getRecord(query, host)
      case "Identify" => identify(host, path)
      case "ListIdentifiers" => listIdentifiers(query)
      case "ListMetadataFormats" => listMetadataFormats(query)
      case "ListRecords" => listRecords(query, host)
      case "ListSets" => listSets(query)
      case _ => error("badVerb", "Unknown verb: " + verb)
    }
  }

  private def getRecord(query: Map[String, String], host: String) = {
    val identifier = query.get("identifier") match {
      case Some(i) => i
      case _ => null
    }
    if (identifier == null) {
       error("badArgument", "Missing identifier")
    } else {
      val metadataPrefix = query.get("metadataPrefix") match {
        case Some(m) => m
        case _ => null
      }
      if (metadataPrefix == null) {
        error("badArgument", "Missing metadataPrefix")
      }  else if (! "oai_dc".equals(metadataPrefix)) {
        error("cannotDisseminateFormat")
      } else {
        Item.findByItemId(identifier.substring("md5:".length)) match {
          case Some(i) => <GetRecord>
                         { itemRecord(i, host, new SidCache()) }
                         </GetRecord>
          case _ => error("idDoesNotExist", "Not found: " + identifier)
        }
      }
    }
  }

  private def identify(host: String, path: String) =
    <Identify>
      <repositoryName>Open Access Topic Hub</repositoryName>
      <baseURL>http://{host}{path}</baseURL>
      <protocolVersion>2.0</protocolVersion>
      <adminEmail>admin@die-spammer.org</adminEmail>
      <earliestDatestamp>{iso8601.print(earliestDatestamp)}</earliestDatestamp>
      <deletedRecord>no</deletedRecord>
      <granularity>YYYY-MM-DD</granularity>
    </Identify>

  private def listIdentifiers(query: Map[String, String]) = {
    val cache = new SidCache()
    <ListIdentifiers>
      { for (item <- itemQuery(query.get("from"), query.get("until"), query.get("set")))
          yield itemHeader(item, cache)
      }
    </ListIdentifiers>
  }

  /**
   * For now, only the required 'oai_dc' format supported for all items: no identifier-specific formats
   */
  private def listMetadataFormats(query: Map[String, String]) = 
    <ListMetadataFormats>
      <metadataFormat>
        <metadataPrefix>oai_dc</metadataPrefix>
        <schema>http://www.openarchives.org/OAI/2.0/oai_dc.xsd</schema>
        <metadataNamespace>http://www.openarchives.org/OAI/2.0/oai_dc/</metadataNamespace>
      </metadataFormat>
    </ListMetadataFormats>

  private def listRecords(query: Map[String, String], host: String) = {
    val cache = new SidCache()
    <ListRecords>
      { for (item <- itemQuery(query.get("from"), query.get("until"), query.get("set")))
          yield itemRecord(item, host, cache)
      }
    </ListRecords>
  }

  private def listSets(query: Map[String, String]) = {
    val cache = new SidCache()
    <ListSets>
      { for (topic <- Topic.all.filter(_.scheme_id != metaSchemeId)) yield
        <set>
          <setSpec>{cache.schemeId(topic.scheme_id)}:{topic.topicId}</setSpec>
          <setName>{topic.name}</setName>
        </set>
      }
    </ListSets>
  }

  private def itemRecord(item: Item, host: String, cache: SidCache) =
    <record>
      { itemHeader(item, cache) }
      <metadata>
        <oai_dc:dc
          xmlns:oai_dc="http://www.openarchives.org/OAI/2.0/oai_dc/"
          xmlns:dc="http://purl.org/dc/elements/1.1/"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://www.openarchives.org/OAI/2.0/oai_dc/ 
          http://www.openarchives.org/OAI/2.0/oai_dc.xsd">
          <dc:title>{item.metadataValue("title")}</dc:title>
          { for (author <- item.metadataValues("author")) yield
             <dc:creator>{author}</dc:creator>
          }
          <dc:identifier>{item.metadataValue("citation")}</dc:identifier>
          <dc:identifier>http://{host}/item/file/{item.id}</dc:identifier>
        </oai_dc:dc>
      </metadata>
    </record>

  private def itemHeader(item: Item, cache: SidCache) =
    <header>
      <identifier>md5:{item.itemId}</identifier>
      <datestamp>{item.created}</datestamp>
      { for (topic <- item.topics.filter(_.scheme_id != metaSchemeId)) yield
        <setSpec>{cache.schemeId(topic.scheme_id)}:{topic.topicId}</setSpec>
      }
    </header>

  private def itemQuery(from: Option[String], until: Option[String], set: Option[String]) = {
    val hits = Item.findByRange(from, until)
    set match {
      case Some(s) => hits.filter(_.topics.contains(set))
      case _ => hits
    }
  }

  private def error(code: String, msg: String = null) = <error code={code}>{msg}</error>

  class SidCache() {
    var sidMap: Map[Long, String] = Map()
    def schemeId(id: Long) = {
      var sid = sidMap.get(id)
      if (sid.isEmpty) {
        val sc = Scheme.findById(id).get; sidMap += (id -> sc.schemeId); sid = Some(sc.schemeId)
      }
      sid.get
    }
  }
}

object OAIHarvester {
  
  def harvest(hid: Int) = {
    ;
  }
}
