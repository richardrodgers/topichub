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

//import play.db.anorm._

import store.{Store, StoredContent}
import models.{Collection, Ctype, Finder, Item, PackageMap, Scheme, Topic}

class CatalogWorker extends Actor {
  def receive = {
    case item: Item => Cataloger.catalog(item)
    case _ => println("bar")
  }
}

class Cataloger(pkgmap: PackageMap, content: StoredContent) {

  // cache documents - so they won't be re-loaded & re-parsed
  var docCache = Map[String, Doc]()
  // cache finder values - to avoid reprocessing of documents
  var infoCache = Map[String, Seq[String]]()
  // count of added topics
  var addedTopics = 0

  def topics(scheme: Scheme, item: Item) = {
    // where in the item package is data for this scheme?
    val (source, format, rank) = pkgmap.mappingsForScheme(scheme).head
    val (idHits, lblHits) = Finder.forSchemeAndFormat(scheme.id, format) match {
      case Some(x) => process(source, x)
      case None => (List(), List())
    }
     // add cardinality checking here
    var idx = 0
    println("IDHits size: " + idHits.size)
    for (id <- idHits) {
      // check for and utilize existing topics
      var tp: Topic = Topic.forSchemeAndId(scheme.schemeId, id) match {
        case Some(x) => x
        case _ => createTopic(scheme, id, lblHits(idx)) //Topic.create(scheme.id, id, lblHits(idx)); Topic.forSchemeAndId(scheme.schemeId, id).get
      }
      item.addTopic(tp)
      addedTopics += 1
      idx += 1
    }
    //idHits foreach (item.addMetadata(label, _))
        /*
        if (topic.title.length == 0) {
          var label = finder.scheme + " " + id
          if (finder.idLabel != null) {
            var labelNS = find(doc, finder.idLabel)
            if (labelNS != null) {
              label = labelNS
            }
          }
          topic.title = label
         */
    /*
      val doc = docToParse(zip, finder.source)
      val id = find(doc, finder.idKey)
      if (id != null) {
        // use existing topics
        var topic: Topic = null
        Topic.find("bySchemeAndTopicId", finder.scheme, id).first match {
          case Some(x) => topic = x
          case _ => topic = new Topic(finder.scheme, id, "")
        }
        mytopics.add(topic)
        if (topic.title.length == 0) {
          var label = finder.scheme + " " + id
          if (finder.idLabel != null) {
            var labelNS = find(doc, finder.idLabel)
            if (labelNS != null) {
              label = labelNS
            }
          }
          topic.title = label
        }
      }
    */
  }

  private def process(source: String, finder: Finder) = {
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
            case Right(x) => val regX(m) = concatText(x); m
            //case Right(x) => val regX(m) = x.firstChild.get.item().value; m
          }
        )
        //idHits = theHits.flatten.toSeq
        idHits = theHits.toSeq
      } else {
        val theHits2 = hits map ( h =>
            h match {
              case Left(x) => x.attribute.value
              case Right(x) => concatText(x)
              // case Right(x) => x.firstChild.get.item().value
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
    // equalize length if needed
    if (idHits.length > lblHits.length) {
      (idHits, lblHits.padTo(idHits.length, "No Label"))
    } else (idHits, lblHits)
  }

  private def concatText(node: XmlPath) = {
    node.foldLeft("")(_+_.item().value)
  }

  def createTopic(scheme: Scheme, topicId: String, title: String): Topic = {
    Topic.create(scheme.id, topicId, title)
    val topic = Topic.forSchemeAndId(scheme.schemeId, topicId).get
    Indexer.index(topic)
    topic
  }

  def metadata(scheme: Scheme, item: Item) {
    // get Finders for this scheme and format
    val label = scheme.schemeId
    val (source, format, rank) = pkgmap.mappingsForScheme(scheme).head
    val (idHits, _) = Finder.forSchemeAndFormat(scheme.id, format) match {
      case Some(x) => process(source, x)
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
            case Right(x) => concatText(x)
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
        val (source, format, rank) = pkgmap.mappingsForScheme(scheme).head
        val finder = Finder.forSchemeAndFormat(scheme.id, format) match {
          case Some(x) => x
          case None => null
        }
        if (finder != null) {
          val (idHits, _) = process(source, finder)
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

object LooseXml extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    //f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    f.setFeature("http://xml.org/sax/features/validation", false);
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
    f.newSAXParser()
  }
}

object Cataloger {

  def catalog(item: Item) = {
    val coll = Collection.findById(item.collection_id).get
    val pkgmap = PackageMap.findById(coll.pkgmap_id).get
    val cataloger = new Cataloger(pkgmap, Store.content(item))
    val ctype = Ctype.findById(item.ctype_id).get
    // start with metadata schemes
    ctype.schemes("meta").foreach( sch => {
      println("Found scheme:" + sch.schemeId)
      cataloger.metadata(sch, item) }
      )
    // next topic schemes
    ctype.schemes("topic").foreach( cataloger.topics(_, item) )
    // now assign to meta-topics as appropriate
    if (cataloger.addedTopics == 0) {
      // assign to 'null' meta-topic
      item.addTopic(Topic.forSchemeAndId("meta", "null") match {
        case Some(x) => x
        case _ => makeTopic("meta", "null", "Items lacking any topic");
      })
      println("No topics")
    } else {
      // assign to the catch-all meta-topic as well
      item.addTopic(Topic.forSchemeAndId("meta", "any") match {
        case Some(x) => x
        case _ => makeTopic("meta", "any", "Items with some topics");
      })
      println("Found some topics")
    }
    // next indexing schemes (which may have already been found as metadata)
    // must follow topic extraction, since items' topics are indexed
    Indexer.index(item, Some(cataloger))
    // finally, should this be delivered on behalf of subscriptions?
    Conveyor.newItem(item)
    //item.changeState("cataloged")
  }

  def contentInfo(item: Item) = {
    val coll = Collection.findById(item.collection_id).get
    val pkgmap = PackageMap.findById(coll.pkgmap_id).get
    val cataloger = new Cataloger(pkgmap, Store.content(item))
    // OK look in map for name of content file in package
    val scheme = Scheme.findByName("meta").get
    val fileInfo = pkgmap.mappingsForScheme(scheme).head
    val filteredName = cataloger.filteredValue(fileInfo._1, 0)
    (filteredName, fileInfo._2)
  }

  def testFinder(item: Item, source: String, finder: Finder) = {
    val coll = Collection.findById(item.collection_id).get
    val pkgmap = PackageMap.findById(coll.pkgmap_id).get
    val cataloger = new Cataloger(pkgmap, Store.content(item))
    cataloger.process(source, finder)
  }

  def makeTopic(schemeId: String, topicId: String, title: String): Topic = {
    val scheme = Scheme.findByName(schemeId).get
    Topic.create(scheme.id, topicId, title);
    Topic.forSchemeAndId(schemeId, topicId).get
  }
}
