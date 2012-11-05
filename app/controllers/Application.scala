/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package controllers

import java.util.{Date}

import akka.actor.Props

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.libs.ws.WS
import play.api.Play.current
import play.api.libs.concurrent._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json._
import play.api.libs.json._

import models._
import store._
import workers.{Cataloger, ConveyorWorker}

object Application extends Controller {

  val indexSvc = Play.configuration.getString("hub.index.url").get

  val conveyor = Akka.system.actorOf(Props[ConveyorWorker], name="conveyor")
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def search = Action { implicit request =>
    val rquery = request.queryString
    val pageSize = 20
    val page = rquery.get("page").getOrElse(List("0")).head.toInt
    val offset = page * pageSize
    val etype = rquery.get("etype").get.head
    val query = rquery.get("query").get.head
    Async {
      val req = WS.url(indexSvc +  etype + "/_search?q=" + query + "&from=" + offset + "&size=" + pageSize)
      req.get().map { response =>
        val json = response.json
        val hits = (json \ "hits" \\ "hits").head \\ "dbId" map(_.as[Long])
        println("Size:" + hits.size)
        hits foreach(x => println(x))
       //val lhits = hits.map(x => x.asInstanceOf[JsString].value.toLong)
        //lhits foreach( x => println(x))
        if ("topic".equals(etype)) {
          val topics = hits map ( id => Topic.findById(id) )
          Ok(views.html.topic_search(topics, page))
        } else {
          val items = hits map ( id => Item.findById(id) )
          Ok(views.html.item_search(items, page))
        }
      }
    }
  }

  def items = Action {
    Ok(views.html.item_index(Publisher.all))
  }

  def item(id: Long) = Action {
    Item.findById(id).map( i => 
      Ok(views.html.item(i))
    ).getOrElse(NotFound("No such item"))
  }

  def itemBrowse(coll_id: Long) = Action {
    Collection.findById(coll_id).map( c => 
      Ok(views.html.item_browse(c))
    ).getOrElse(NotFound("No such collection"))
  }

  def itemFile(id: Long) = Action {
    Item.findById(id).map( i => {
      val itemPkg = Store.content(i)
      SimpleResult(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> itemPkg.mimetype)),
        body = Enumerator.fromStream(itemPkg.content)
      )
    }
    ).getOrElse(NotFound("No such item"))  
  }

  def itemTransfer(id: Long) = Action { implicit request =>
    Item.findById(id).map( i => {
      val sub = Subscriber.findByContact(request.session.get("email").get).get
      Ok(views.html.item_transfer(id, sub.targets))
    }
    ).getOrElse(NotFound("No such item"))
  }

  def transfer(id: Long) = Action { implicit request =>
    Item.findById(id).map( i => {
      val sub = Subscriber.findByContact(request.session.get("email").get).get
      // create a transfer and pass to a conveyor
      val targ_id = request.queryString.get("target").getOrElse(List("0L")).head.toLong
      conveyor ! Transfer.make(targ_id, i.id, -1, Some("unknown"))
      Ok(views.html.item_index(Publisher.all))
    }
    ).getOrElse(NotFound("No such item"))
  }

  def topics = Action {
    Ok(views.html.topic_index(Scheme.withGentype("topic")))
  }

  def topic(id: Long) = Action {
    Topic.findById(id).map( t => 
      Ok(views.html.topic(t))
    ).getOrElse(NotFound("No such topic"))
  }

  def topicBrowse(scheme_id: Long) = Action {
    Scheme.findById(scheme_id).map( s => 
      Ok(views.html.topic_browse(s))
    ).getOrElse(NotFound("No such scheme"))
  }

  def topicSubscribe(id: Long) = Action { implicit request =>
    Topic.findById(id).map( t => {
      val sub = Subscriber.findByContact(request.session.get("email").get).get
      Ok(views.html.topic_subscribe(id, sub.targets))
    }
    ).getOrElse(NotFound("No such topic"))
  }

  def subscribe(id: Long) = Action { implicit request =>
    Topic.findById(id).map( t => {
      val sub = Subscriber.findByContact(request.session.get("email").get).get
      // create a subscription and pass to a conveyor
      val targ_id = request.queryString.get("target").getOrElse(List("0L")).head.toLong
      conveyor ! Subscription.make(sub.id, targ_id, id, "all")
      Ok(views.html.item_index(Publisher.all))
    }
    ).getOrElse(NotFound("No such topic"))
  }

  val pubForm = Form(
  	mapping(
      "id" -> ignored(0L),
      "pubId" -> nonEmptyText,
      "name" -> nonEmptyText,
      "description" -> nonEmptyText,
      "role" -> nonEmptyText,
      "home" -> optional(text),
      "logo" -> optional(text),
      "created" -> ignored(new Date)
    )(Publisher.apply)(Publisher.unapply)
  )

  def publishers = Action {
  	Ok(views.html.pub_list(Publisher.all))
  }

  def newPublisher = Action {
    Ok(views.html.new_publisher(null, pubForm))
  }

  def publisher(id: Long) = Action {
    Publisher.findById(id).map( p => 
      Ok(views.html.pub(p))
    ).getOrElse(NotFound("No such publisher"))
  }

  def createPublisher = Action { implicit request =>
    pubForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_publisher(null, errors)),
      value => {
        Publisher.create(value.pubId, value.name, value.description, value.role, value.home, value.logo)
        Redirect(routes.Application.publishers)
      }
    )
  }

  val collForm = Form(
    mapping(
      "id" -> ignored(0L),
      "publisher_id" -> ignored(1L),
      "ctype_id" -> longNumber,
      "pkgmap_id" -> longNumber,
      "description" -> nonEmptyText,
      "policy" -> nonEmptyText,
      "created" -> ignored(new Date),
      "updated" -> ignored(new Date),
      "deposits" -> ignored(0)
    )(Collection.apply)(Collection.unapply)
  )

  def newCollection(id: Long) = Action {
     Publisher.findById(id).map( p => 
        Ok(views.html.new_collection(p, collForm))
      ).getOrElse(NotFound("No such publisher"))
  }

  def createCollection(id: Long) = Action { implicit request =>
    collForm.bindFromRequest.fold(
      errors => {
        val p = Publisher.findById(id).get
        BadRequest(views.html.new_collection(p, errors))
      },
      value => {
        Collection.create(id, value.ctype_id, value.pkgmap_id, value.description, value.policy)
        Redirect(routes.Application.publisher(id))
      }
    )
  }

  val schemeForm = Form(
    mapping(
      "id" -> ignored(0L),
      "schemeId" -> nonEmptyText,
      "gentype" -> nonEmptyText,
      "category" -> nonEmptyText,
      "description" -> nonEmptyText,
      "home" -> optional(text),
      "logo" -> optional(text),
      "created" -> ignored(new Date)
    )(Scheme.apply)(Scheme.unapply)
  )

  def schemes = Action {
    Ok(views.html.scheme_list(Scheme.all))
  }

  def scheme(id: Long) = Action {
    Scheme.findById(id).map( s => 
      Ok(views.html.scheme(s))
    ).getOrElse(NotFound("No such scheme"))
  }

  def newScheme = Action {
     Ok(views.html.new_scheme(schemeForm))
  }

  def createScheme = Action { implicit request =>
   schemeForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_scheme(errors)),
      value => {
        Scheme.create(value.schemeId, value.gentype, value.category, value.description, value.home, value.logo)
        Redirect(routes.Application.schemes)
      }
    )
  }
  // content types
  val ctypeForm = Form(
    mapping(
      "id" -> ignored(0L),
      "ctypeId" -> nonEmptyText,
      "description" -> nonEmptyText,
      "logo" -> optional(text)
    )(Ctype.apply)(Ctype.unapply)
  )

  def ctypes = Action {
    Ok(views.html.ctype_list(Ctype.all))
  }

  def ctype(id: Long) = Action {
    Ctype.findById(id).map( t => 
      Ok(views.html.ctype(t, ctSchemeForm))
    ).getOrElse(NotFound("No such content type"))
  }

  def newCtype = Action {
     Ok(views.html.new_ctype(ctypeForm))
  }

  def createCtype = Action { implicit request =>
    ctypeForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_ctype(errors)),
      value => {
        Ctype.create(value.ctypeId, value.description, value.logo)
        Redirect(routes.Application.ctypes)
      }
    )
  }

  // package maps
  val pkgmapForm = Form(
    mapping(
      "id" -> ignored(0L),
      "pkgmapId" -> nonEmptyText,
      "description" -> nonEmptyText,
      "swordurl" -> optional(text)
    )(PackageMap.apply)(PackageMap.unapply)
  )

  def pkgmaps = Action {
    Ok(views.html.pkgmap_list(PackageMap.all))
  }

  def pkgmap(id: Long) = Action {
    PackageMap.findById(id).map( m => 
      Ok(views.html.pkgmap(m, pmSchemeForm))
    ).getOrElse(NotFound("No such package map"))
  }

  def newPkgmap = Action {
     Ok(views.html.new_pkgmap(pkgmapForm))
  }

  def createPkgmap = Action { implicit request =>
    pkgmapForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_pkgmap(errors)),
      value => {
        PackageMap.create(value.pkgmapId, value.description, value.swordurl)
        Redirect(routes.Application.pkgmaps)
      }
    )
  }

  val pmSchemeForm = Form(
    tuple(
      "scheme_id" -> longNumber,
      "source" -> nonEmptyText,
      "format" -> nonEmptyText,
      "rank" -> ignored(0L)
    )
  )

  def newPkgmapMapping(id: Long) = Action { implicit request =>
    pmSchemeForm.bindFromRequest.fold(
      errors => {
        val pm = PackageMap.findById(id).get
        BadRequest(views.html.pkgmap(pm, errors))
      },
      value => {
        val pm2 = PackageMap.findById(id).get
        val (scheme_id, source, format, _) = pmSchemeForm.bindFromRequest.get
        val scheme = Scheme.findById(scheme_id).get
        pm2.addMapping(scheme, source, format, 0)
        Redirect(routes.Application.pkgmap(id))
      }
    )
  }

  val ctSchemeForm = Form(
    tuple(
      "scheme_id" -> longNumber,
      "foo" -> ignored(0L)
    )
  )

  def newCtypeMD(id: Long) = Action { implicit request =>
    ctSchemeForm.bindFromRequest.fold(
      errors => {
        val ct = Ctype.findById(id).get
        BadRequest(views.html.ctype(ct, errors))
      },
      value => {
        val ct2 = Ctype.findById(id).get
        val (scheme_id, _) = ctSchemeForm.bindFromRequest.get
        val scheme = Scheme.findById(scheme_id).get
        ct2.addScheme(scheme, "meta")
        Redirect(routes.Application.ctype(id))
      }
    )
  }

  def newCtypeIndex(id: Long) = Action { implicit request =>
    ctSchemeForm.bindFromRequest.fold(
      errors => {
        val ct = Ctype.findById(id).get
        BadRequest(views.html.ctype(ct, errors))
      },
      value => {
        val ct2 = Ctype.findById(id).get
        val (scheme_id, _) = ctSchemeForm.bindFromRequest.get
        val scheme = Scheme.findById(scheme_id).get
        ct2.addScheme(scheme, "index")
        Redirect(routes.Application.ctype(id))
      }
    )
  }

  def newCtypeTopic(id: Long) = Action { implicit request =>
    ctSchemeForm.bindFromRequest.fold(
      errors => {
        val ct = Ctype.findById(id).get
        BadRequest(views.html.ctype(ct, errors))
      },
      value => {
        val ct2 = Ctype.findById(id).get
        val (scheme_id, _) = ctSchemeForm.bindFromRequest.get
        val scheme = Scheme.findById(scheme_id).get
        ct2.addScheme(scheme, "topic")
        Redirect(routes.Application.ctype(id))
      }
    )
  }

  val finderForm = Form(
    mapping(
      "id" -> ignored(0L),
      "scheme_id" -> longNumber,
      "description" -> nonEmptyText,
      "cardinality" -> nonEmptyText,
      "format" -> nonEmptyText,
      "idKey" -> nonEmptyText,
      "idLabel" -> nonEmptyText,
      "author" -> nonEmptyText,
      "created" -> ignored(new Date)
    )(Finder.apply)(Finder.unapply)
  )

  def finders(schemeId: String) = Action {
    Scheme.findByName(schemeId).map( scheme =>
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    ).getOrElse(NotFound("Unknown scheme"))
  }

  def newFinder(schemeId: String) = Action {
    Scheme.findByName(schemeId).map( scheme =>
      Ok(views.html.new_finder(schemeId, finderForm))
    ).getOrElse(NotFound("Unknown scheme"))
  }

  def createFinder(schemeId: String) = Action { implicit request =>
    Scheme.findByName(schemeId).map( scheme =>
      finderForm.bindFromRequest.fold(
        errors => BadRequest(views.html.new_finder(schemeId, errors)),
        value => {
          Finder.create(scheme.id, value.description, value.cardinality, value.format, value.idKey, value.idLabel, value.author)
          Redirect(routes.Application.finders(schemeId))
        }
      )
    ).getOrElse(NotFound)
  }

  def deleteFinder(schemeId: String, id: Long) = Action {
    Scheme.findByName(schemeId).map( scheme => {
      Finder.delete(id)
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    }
    ).getOrElse(NotFound("Unknown scheme"))
  }

  val finderTestForm = Form(
    tuple(
      "itemId" -> longNumber,
      "source" -> text,
      "idKey" -> text,
      "idLabel" -> text
    )
  )

  def testFinder = Action {
    Ok(views.html.test_finder(finderTestForm, List[String]()))
  }

  def runTestFinder = Action { implicit request => {
    val (itemId, source, idKey, idLabel) = finderTestForm.bindFromRequest.get
    val finder = Finder(0L, 0L, "", "1", "epdcx", idKey, idLabel, "RLR", new Date)
    val item = Item.findById(itemId).get
    val (idHits, ldlHits) = Cataloger.testFinder(item, source, finder)
    //val theMap = Map("source" -> source, "idKey" -> idKey, "idLabel" -> idLabel)
    //finderTestForm.bind(theMap)
    Ok(views.html.test_finder(finderTestForm, idHits)) }
  }


/*
  def runTestFinder = Action { implicit request =>
    finderTestForm.bindFromRequest.fold (
      errors => BadRequest(views.html.test_finder(errors, List[String]())),
      (itemId, source, idKey, idLabel) => {
        val finder = Finder(0L, 0L, "", "1", "epdcx", source, idKey, idLabel)
        val item = Item.findById(itemId).get
        val (idHits, ldlHits) = Cataloger.testFinder(item, finder)
        Ok(views.html.test_finder(finderTestForm, idHits))
      }
    )
  }
  */

  val subscriberForm = Form(
    mapping(
      "id" -> ignored(0L),
      "userId" -> nonEmptyText,
      "password" -> nonEmptyText,
      "home" -> optional(text),
      "logo" -> optional(text),
      "role" -> nonEmptyText,
      "contact" -> nonEmptyText,
      "swordService" -> nonEmptyText,
      "terms" -> nonEmptyText,
      "backFile" -> nonEmptyText,
      "created" -> ignored(new Date)
    )(Subscriber.apply)(Subscriber.unapply)
  )

  def subscribers = Action {
    Ok(views.html.subscriber_list(Subscriber.all))
  }

  def subscriber(id: Long) = Action {
    Subscriber.findById(id).map( s => 
      Ok(views.html.subscriber(s))
    ).getOrElse(NotFound("No such subscriber"))
  }

  def newSubscriber = Action {
    Ok(views.html.new_subscriber(subscriberForm))
  }

  def createSubscriber = Action { implicit request =>
    subscriberForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_subscriber(errors)),
      value => {
        Subscriber.create(value.userId, value.password, value.home, value.logo, value.role, value.contact, value.swordService, value.terms, value.backFile)
        Redirect(routes.Application.subscribers)
      }
    )
  }

  val targetForm = Form(
    mapping(
      "id" -> ignored(0L),
      "subscriber_id" -> ignored(1L),
      "description" -> nonEmptyText,
      "userId" -> nonEmptyText,
      "password" -> nonEmptyText,
      "depositUrl" -> nonEmptyText,
      "created" -> ignored(new Date),
      "updated" -> ignored(new Date),
      "tarnsfers" -> ignored(0)
    )(Target.apply)(Target.unapply)
  )

  def target(id: Long) = Action {
    Target.findById(id).map( t => 
      Ok(views.html.target(t))
    ).getOrElse(NotFound("No such subscriber destination"))
  }

  def newTarget(sid: Long) = Action {
    Ok(views.html.new_target(sid, targetForm))
  }

  def createTarget(sid: Long) = Action { implicit request =>
    targetForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_target(sid, errors)),
      value => {
        Target.create(sid, value.description, value.userId, value.password, value.depositUrl)
        Redirect(routes.Application.subscriber(sid))
      }
    )
  }

  def subscription(id: Long) = Action {
    Subscription.findById(id).map( s => 
      Ok(views.html.subscription(s))
    ).getOrElse(NotFound("No such subscription"))
  }
}
