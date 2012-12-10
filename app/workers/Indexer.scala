/**
  * Copyright 2012 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import scala.collection.Set
import scala.collection.mutable.HashSet
import scala.xml.{Elem, Node, NodeSeq, XML}
import scala.util.matching.Regex
import scala.xml.factory.XMLLoader

import akka.actor.Actor

import java.io.{File, InputStream}
import java.util.Date

import javax.xml.parsers.SAXParser

import org.xml.sax.InputSource

import scales.xml.jaxen._
import scales.xml._
import scales.utils._
import ScalesUtils._
import ScalesXml._

import play.api._
import play.api.libs.json._
import play.api.libs.json.Json
import play.api.libs.ws.WS

//import play.db.anorm._

import store.{Store, StoredContent}

import models.{Collection, Ctype, Finder, Item, PackageMap, Scheme, Topic}

class IndexWorker extends Actor {
  def receive = {
    case item: Item => Indexer.index(item, None)
    case topic: Topic => Indexer.index(topic)
    case dtype: String => Indexer.reindex(dtype)
    case _ => println("bar")
  }
}

class Indexer(format: String, content: StoredContent) {

  // cache documents - so they won't be re-loaded & re-parsed
  var docCache = Map[String, Doc]()
  // cache finder values - to avoid reprocessing of documents
  var infoCache = Map[String, Seq[String]]()
  // count of added topics
  var addedTopics = 0

  def topics(scheme: Scheme, item: Item) = {
    val (idHits, lblHits) = Finder.forSchemeAndFormat(scheme.id, format) match {
      case Some(x) => process(x)
      case None => (List(), List())
    }
     // add cardinality checking here
    var idx = 0
    println("IDHits size: " + idHits.size)
    for (id <- idHits) {
      // check for and utilize existing topics
      var tp: Topic = Topic.forSchemeAndId(scheme.schemeId, id) match {
        case Some(x) => x
        case _ => Topic.create(scheme.id, id, lblHits(idx)); Topic.forSchemeAndId(scheme.schemeId, id).get
      }
      item.addTopic(tp)
      addedTopics += 1
      idx += 1
    }
  }

  def process(finder: Finder) = {
    val source = "replaceme"
    val doc = docToParse(source)
    var idHits: Seq[String]= null
    var lblHits: Seq[String] = null
    if (doc != null) {
      // do Id & label
      println("in process about to evaluate: " + finder.idKey)
      var keyParts = finder.idKey.split(" ")
      // new way
      println("keyParts0: " + keyParts(0))
      val xp = new ScalesXPath(keyParts(0)).withNameConversion(ScalesXPath.localOnly)
      val hits = xp.evaluate(top(doc))
      println("Post eval num hits: " + hits.size)
      if (keyParts.length == 2) {
        val regX = keyParts(1).r
        val theHits = hits map ( h =>
          h match {
            case Left(x) => val regX(l) = x.attribute.value; l
            //case Right(x) => regX findFirstIn x.firstChild.get.item().value
            case Right(x) => val regX(m) = x.firstChild.get.item().value; m
          }
        )
        //idHits = theHits.flatten.toSeq
        idHits = theHits.toSeq
      } else {
        val theHits2 = hits map ( h =>
            h match {
              case Left(x) => x.attribute.value
              case Right(x) => x.firstChild.get.item().value
            }
          )
        idHits = theHits2.toSeq
      }
    }
      /*
      idHits = XPath.evaluate(keyParts(0), doc)
      if (keyParts.length == 2) {
        // hits need to be transformed by Regex
        var Transformer = new Regex(keyParts(1))
        var transList = List[String]()
        for (hit <- idHits) {
          var Transformer(x) = hit
          //var result:String = x
          transList = x :: transList
        }
        var revList = transList.reverse
        idHits = revList
        */
        // also stow in infoCache
        idHits.foreach(println)
        //infoCache += ("id" -> idHits)
      //}
      val idl = finder.idLabel
      // if idl is an XPath, evaluate it
      if (idl != null && idl.length > 0 && idl.indexOf("/") >= 0) {
        //val xpl = new ScalesXPath(idl, Map())
        val source = "replaceme"
        lblHits = xpathFind(idl, source)
        //lblHits =  XPath.evaluate(idl, doc)
      } else if (idl != null && idl.length > 0) {
        println("process filtered value; " + filteredValue(idl, 0))
        var lblList = List[String]()
        var count = 0
        for (a <- idHits) {
           lblList = filteredValue(idl, count) :: lblList
           count += 1
        }
        //var revLblList = lblList.reverse
        lblHits = lblList.reverse
      } else
        lblHits = List("No label")
    //}
    (idHits, lblHits)
  }

  def metadata(scheme: Scheme, item: Item) {
    // get Finders for this scheme and format
    val label = scheme.schemeId
    val (idHits, _) = Finder.forSchemeAndFormat(scheme.id, format) match {
      case Some(x) => process(x)
      case None => (List("Unknown " + label), List())
    }
    idHits foreach (item.addMetadata(label, _))
  }

  def xpathFind(expr: String, source: String) = {
    val xp = new ScalesXPath(expr, Map())
    val hits = xp.evaluate(top(docToParse(source))) 
    val theHits = hits map ( h =>
          h match {
            case Left(x) => x.attribute.value
            case Right(x) => x.firstChild.get.item().value
          }
        )
    //theHits.flatten.toSeq
    theHits.toSeq
  }

  def filteredValue(token: String, index: Int): String = {
    val start = token.indexOf("{")
    if (start >= 0) {
      val end = token.indexOf("}", start + 1)
      token.substring(0, start) + filter(token.substring(start + 1, end), index) + filteredValue(token.substring(end + 1), index)
    } else
      token
  }

  def filter(token: String, index: Int) = {
    // is value cached?
    var value = infoCache.get(token) match {
      case Some(x) =>
                      println("In filter token: " + token + " index: " + index + " size: " + x.size)
                      x(index)
      case _ => null
    }
    if (value == null) {
      val scheme = Scheme.findByName(token).get
      if (scheme != null) {
        val finder = Finder.forSchemeAndFormat(scheme.id, format) match {
          case Some(x) => x
          case None => null
        }
        if (finder != null) {
          val (idHits, _) = process(finder)
          //val doc =  docToParse(zip, finder.source)
          //if (doc != null) {
          // value = find(doc, finder.idKey)
          if (idHits.size > 0) {
            value = idHits(index)
            var valList: List[String] = List()
            for (idHit <- idHits) {
              println("idHit: " + idHit)
              valList = idHit :: valList
            }
            infoCache += (token -> valList.reverse)
          } else
            value = "Unknown value"
        }
      }
    }
    value
  }

  def docToParse(name: String) = {
    val fname = filteredValue(name, 0)
    println("doc2p: fname: " + fname)
    // check doc cache first
    docCache.get(fname) match {
      case Some(x) => x
      case _ => val doc = stupidLoad(content.resource(fname)); docCache += (fname -> doc); doc
      //case _ => val doc = loadXml(new InputSource(content.resource(fname))); docCache += (fname -> doc); doc
      // sigh - resorting to unbelievably stupid parsing work-around. Currently cannot determine how to configure
      // scales xml loader to ignore DTD, so doing a scala xml load, then converting
    }
  }

  def stupidLoad(in: InputStream) = {
    val scalaDoc = LooseXml.load(in)
    convertFromScalaXml(scalaDoc)
  }
}

object Indexer {

  import play.api.Play.current
  import play.api.libs.json.Json._

  val indexSvc = Play.configuration.getString("hub.index.url").get

  def reindex(dtype: String) = {
    val delreq = WS.url(indexSvc + dtype)
    delreq.delete()
    if ("topic".equals(dtype)) {
      Topic.all.foreach(index(_))
    } //else if ("item".equals(dtype)) {
    //  Item.all.foreach(index(_))
    //}
  }

  def index(topic: Topic) = {
    // minimal indexing: dbId, schemeId, topicId, and title
    val data = Map("dbId" -> toJson(topic.id),
                   "schemeId" -> toJson(topic.scheme.get.schemeId),
                   "topicId" -> toJson(topic.topicId),
                   "name" -> toJson(topic.name))
    val jdata = stringify(toJson(data))
    // debug
    println("Topic index: " + jdata)
    val req = WS.url(indexSvc + "topic/" + topic.id)
    req.put(jdata)
  }

  def index(item: Item, cataloger: Option[Cataloger]) = {
    cataloger match {
      case Some(x) => index0(item, x)
      case _ => {
        val coll = Collection.findById(item.collection_id).get
        val pkgmap = PackageMap.findById(coll.pkgmap_id).get
        index0(item, new Cataloger(pkgmap, Store.content(item)))
      }
    }
  }

  private def index0(item: Item, cataloger: Cataloger) = {
    var dataMap = Map[String, JsValue]()
    val ctype = Ctype.findById(item.ctype_id).get
    dataMap += "dbId" -> toJson(item.id)
    // add all defined index fields
    ctype.schemes("index").foreach(dataMap += addMetadata(_, item))
    // also add all topics
    dataMap += "topicSchemeId" -> toJson(item.topics.map(_.scheme.get.schemeId))
    dataMap += "topicId" -> toJson(item.topics.map(_.topicId))
    val req = WS.url(indexSvc + "item/" + item.id)
    req.put(stringify(toJson(dataMap)))
  }

  private def addMetadata(scheme: Scheme, item: Item) = {
    // NB: need logic here to test whether scheme is metadata or not
    scheme.schemeId -> toJson(item.metadataValue(scheme.schemeId))
  }
}

object Searcher {

  import play.api.Play.current
  import play.api.libs.json.Json._

  val indexSvc = Play.configuration.getString("hub.index.url")

  def itemSearch(query: String, page: Int = 0) = {
    // pass query to index service and manage results
    println("got query: " + query)
    val pageSize = "20"
    val offset = page * pageSize.toInt
    val req = WS.url(indexSvc + "item/_search?q=" + query + "&from=" + offset + "&size=" + pageSize)
    /*
    Async {
      req.get().map { response =>
        val json = response.json
        val jsonObj = json.getAsJsonObject
        val topHits = jsonObj.getAsJsonObject("hits")
        val theHits = topHits.getAsJsonArray("hits")
      }
    }
    */
    //val resp = req.get()
    //val json = Json.parse(resp)
    //println("got status: " + resp.getStatus)
    //val jsonObj = json.getAsJsonObject
    //val topHits = jsonObj.getAsJsonObject("hits")
    //val theHits = topHits.getAsJsonArray("hits")
    /*
    var itemList2 = List[Item]()
    for (i <- 0 to theHits.size - 1) {
      val hit = theHits.get(i)
      var id = hit.getAsJsonObject().getAsJsonPrimitive("_id").getAsInt
      println("Got id: " + id)
      itemList2 = Item.findById(id).get :: itemList2
    }
    println("itemList size: " + itemList2.size)
    var itemList = itemList2.toArray
    */
    //html.itemSearch(query, itemList, page)
  }


}
