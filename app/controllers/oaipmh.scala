/**
  * Copyright 2012 MIT Libraries
  * Licensed under:  http://www.apache.org/licenses/LICENSE-2.0
  */
package controllers

import scala.xml.{Attribute, Elem, NodeSeq, Text, Null}

import java.text.SimpleDateFormat
import java.util.Date

import org.joda.time.DateTimeZone
import org.joda.time.format.ISODateTimeFormat

import play.api.libs.iteratee.Enumerator
import play.api._
import play.api.Play.current
import play.api.mvc._

import models.{Item, Scheme, Topic}

/**
  * OAI object manages OAI-PMH (Open Archives Initiative - Protocol for Metadata
  * Harvesting) v2.0 protocol operations. The hub is a 'repository' (data provider)
  * in OAI-PMH terms, meaning it responds in the protocol-ordained manner to
  * 'harvesters' requests. Data model: OAI Items = Items, and OAI Sets = Topics
  *
  * @author richardrodgers
  */

object OAI extends Controller {

  val recLimit = 100
  val adminEmail = Play.configuration.getString("hub.admin.email").get
  val iso8601 = ISODateTimeFormat.dateTimeNoMillis.withZone(DateTimeZone.UTC)
  val prFormat = new SimpleDateFormat("yyyy-MM-dd")
  val tsFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  lazy val metaSchemeId = Scheme.findByName("meta").map(sc => sc.id).getOrElse(0L)
  lazy val earliestDatestamp = Item.findOldest.map(item => item.created.getTime()).getOrElse((new Date).getTime)

  def provide = Action { implicit request =>
    val query = request.queryString.mapValues(_.head)
    SimpleResult(
      header = ResponseHeader(200, Map(CONTENT_TYPE -> "text/xml")),
      body = Enumerator(
        <OAI-PMH xmlns="http://www.openarchives.org/OAI/2.0/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.openarchives.org/OAI/2.0/
           http://www.openarchives.org/OAI/2.0/OAI-PMH.xsd">
          <responseDate>{iso8601.print(System.currentTimeMillis())}</responseDate>
          { withAttrs(<request>http://{request.host}{request.path}</request>, query) }
          { if (query.get("verb").isDefined) processQuery(query, request.host, request.path) else error("badVerb", "Missing verb") }
        </OAI-PMH>)
      )
  }

  private def processQuery(query: Map[String, String], host: String, path: String): NodeSeq = {
    val verb = query.get("verb").get
    try {
      verb match {
        case "GetRecord" => getRecord(query, host)
        case "Identify" => identify(host, path)
        case "ListIdentifiers" => listIdentifiers(query)
        case "ListMetadataFormats" => listMetadataFormats(query)
        case "ListRecords" => listRecords(query, host)
        case "ListSets" => listSets(query)
        case _ => error("badVerb", "Unknown verb: " + verb)
      }
    } catch {
      case ex: IllegalStateException => error("noRecordsMatch")
      case _ => error("badArgument")
    }
  }

  private def getRecord(query: Map[String, String], host: String) = {
    query.get("identifier").map( identifier => 
      query.get("metdataPrefix").map( metadataPrefix => 
        if ("oai_dc".equals(metadataPrefix))
          Item.findByItemId(identifier.substring("md5:".length)).map( item =>
            <GetRecord> 
             { itemRecord(item, host, new SidCache()) }
            </GetRecord>
          ).getOrElse(error("idDoesNotExist", "Not found: " + identifier))
        else error("cannotDisseminateFormat")
      ).getOrElse(error("badArgument", "Missing metadataPrefix"))
    ).getOrElse(error("badArgument", "Missing identifier"))
  }

  private def identify(host: String, path: String) =
    <Identify>
      <repositoryName>Open Access Topic Hub</repositoryName>
      <baseURL>http://{host}{path}</baseURL>
      <protocolVersion>2.0</protocolVersion>
      <adminEmail>{adminEmail}</adminEmail>
      <earliestDatestamp>{iso8601.print(earliestDatestamp)}</earliestDatestamp>
      <deletedRecord>no</deletedRecord>
      <granularity>YYYY-MM-DD</granularity>
    </Identify>

  private def listIdentifiers(query: Map[String, String]) = {
    val cache = new SidCache()
    val hits = itemQuery(query)
    val results = if (hits.length > recLimit) hits.dropRight(1) else hits
    <ListIdentifiers>
      { for (item <- results)
          yield itemHeader(item, cache)
      } {
        if (hits.length > recLimit)
          { withAttrs(<resumptionToken>{results(recLimit-1).created}</resumptionToken>,
                      Map("completeListSize" -> Item.count.toString)) }
        else if (query.get("resumptionToken").isDefined)
          { withAttrs(<resumptionToken/>, Map("completeListSize" -> Item.count.toString)) }
      }
    </ListIdentifiers>
  }

  // For now, only the required 'oai_dc' format supported for all items: no identifier-specific formats
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
    val hits = itemQuery(query)
    val results = if (hits.length > recLimit) hits.dropRight(1) else hits
    <ListRecords>
      { for (item <- results)
          yield itemRecord(item, host, cache)
      } {
        if (hits.length > recLimit)
          { withAttrs(<resumptionToken>{results(recLimit-1).created}</resumptionToken>,
                      Map("completeListSize" -> Item.count.toString)) }
        else if (query.get("resumptionToken").isDefined)
          { withAttrs(<resumptionToken/>, Map("completeListSize" -> Item.count.toString)) }
      }
    </ListRecords>
  }

  private def listSets(query: Map[String, String]) = {
    val cache = new SidCache()
    val token = query.get("resumptionToken")
    val start = token.map(t => tsFormat.parse(t)).getOrElse(new Date(earliestDatestamp - 1000))
    // look-ahead by 1 to see if we need to paginate results
    val hits = Topic.since(start, recLimit + 1)
    val results = if (hits.length > recLimit) hits.dropRight(1) else hits
    <ListSets>
      { for (topic <- results.filter(_.scheme_id != metaSchemeId)) yield
        <set>
          <setSpec>{cache.schemeId(topic.scheme_id)}:{topic.topicId}</setSpec>
          <setName>{topic.name}</setName>
        </set>
      } {
        if (hits.length > recLimit)
          { withAttrs(<resumptionToken>{results(recLimit-1).created}</resumptionToken>,
                      Map("completeListSize" -> Topic.count.toString)) }
        else if (token.isDefined)
          { withAttrs(<resumptionToken/>, Map("completeListSize" -> Topic.count.toString)) }
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
          <dc:identifier>http://{host}/item/view/{item.id}</dc:identifier>
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

  private def itemQuery(query: Map[String, String]) = {
    val earliest = query.get("from").map(f => prFormat.parse(f)).getOrElse(new Date(earliestDatestamp - 1000))
    val fromToken = query.get("resumptionToken").map(t => tsFormat.parse(t)).getOrElse(earliest)
    val latest = query.get("until").map(u => prFormat.parse(u)).getOrElse(new Date())
    // look-ahead by 1 to see if we need to paginate results
    query.get("set").map( s => {
      Topic.forQualifiedId(s).map( topic =>
        Item.findByRangeTopic(fromToken, latest, topic.id, recLimit + 1)
      ).getOrElse(throw new IllegalStateException())
    }).getOrElse(Item.findByRange(fromToken, latest, recLimit + 1))
  }

  private def withAttrs(elem: Elem, attrs: Map[String, String]) = {
    var wAttrs = elem
    for (arg <- attrs.keysIterator) {
      wAttrs = wAttrs % Attribute(None, arg, Text(attrs.get(arg).get), Null)
    }
    wAttrs
  }

  private def error(code: String, msg: String = null) = <error code={code}>{msg}</error>

  class SidCache() {
    var sidMap: Map[Long, String] = Map()
    def schemeId(id: Long) = {
      var sid = sidMap.get(id)
      if (sid.isEmpty) {
        val sc = Scheme.findById(id).get
        sidMap += (id -> sc.schemeId)
        sid = Some(sc.schemeId)
      }
      sid.get
    }
  }
}
