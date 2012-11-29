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
import workers.{Cataloger, ConveyorWorker, IndexWorker}

object Application extends Controller {

  val indexSvc = Play.configuration.getString("hub.index.url").get

  val conveyor = Akka.system.actorOf(Props[ConveyorWorker], name="conveyor")
  val indexer = Akka.system.actorOf(Props[IndexWorker], name="indexer")
  
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
          Ok(views.html.item_search(items, query, page))
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

  def itemBrowse(filter: String, id: Long, page: Int) = Action {
    filter match {
      case "collection" => itemBrowseCollection(id, page)
      case "topic" => itemBrowseTopic(id, page)
      case _ => NotFound("No such filter")
    }
  }

  private def itemBrowseTopic(id: Long, page: Int) = {
    Topic.findById(id).map( topic => 
      Ok(views.html.item_browse(id, topic.pagedItems(page, 10), "topic", topic.title, page, topic.itemCount))
    ).getOrElse(NotFound("No such topic"))
  }

  private def itemBrowseCollection(id: Long, page: Int) = {
    Collection.findById(id).map( coll => 
      Ok(views.html.item_browse(id, Item.inCollection(id, page), "collection", coll.description, page, Item.collectionCount(coll.id)))
    ).getOrElse(NotFound("No such collection"))
  }

  def itemSearch(topicId: String, page: Int) = Action { implicit request =>
    val pageSize = 20
    val offset = page * pageSize
    Async {
      val req = WS.url(indexSvc + "item/_search")
      //val req = WS.url(indexSvc + "item/_search?from=" + offset + "&size=" + pageSize)
      //val req = WS.url(indexSvc + "item/_search?q=topicId:" + topicId + "&from=" + offset + "&size=" + pageSize)
      val qbody = toJson(Map("query" -> toJson(Map("term" -> toJson(Map("topicId" -> topicId))))))
      println("query: "  + Json.stringify(qbody))
      req.post(Json.stringify(qbody)).map { response =>
        val json = response.json
        val hits = (json \ "hits" \\ "hits").head \\ "dbId" map(_.as[Long])
        println("Size:" + hits.size)
        hits foreach(x => println(x))
        //val lhits = hits.map(x => x.asInstanceOf[JsString].value.toLong)
        //lhits foreach( x => println(x))
        val items = hits map ( id => Item.findById(id) )
        val query = "topicId:" + topicId
        Ok(views.html.item_search(items, query, page))
      }
    }
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

  def itemView(id: Long) = Action { implicit request =>
    Item.findById(id).map( i => {
      val (name, mimeType) = Cataloger.contentInfo(i)
      SimpleResult (
        header =  ResponseHeader(200, Map(CONTENT_TYPE -> mimeType)),
        body = Enumerator.fromStream(Store.content(i).resource(name))
      )
    }
    ).getOrElse(NotFound("No such item"))
  }

  def itemTransfer(id: Long, mode: String) = Action { implicit request =>
    Item.findById(id).map( item => {
      val sub = Subscriber.findByContact(request.session.get("email").get).get
      Ok(views.html.item_transfer(id, sub.targetsWith(mode)))
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

  def topicBrowse(scheme_id: Long, page: Int) = Action {
    Scheme.findById(scheme_id).map( scheme => 
      Ok(views.html.topic_browse(scheme.id, Topic.withScheme(scheme.id, page), scheme.description, page, scheme.topicCount))
    ).getOrElse(NotFound("No such scheme"))
  }

  def topicSubscribe(id: Long, mode: String) = Action { implicit request =>
    Topic.findById(id).map( t => {
      val sub = Subscriber.findByContact(request.session.get("email").get).get
      Ok(views.html.topic_subscribe(id, sub.targetsWith(mode)))
    }
    ).getOrElse(NotFound("No such topic"))
  }

  def subscribe(id: Long) = Action { implicit request =>
    Topic.findById(id).map( t => {
      val sub = Subscriber.findByContact(request.session.get("email").get).get
      // create a subscription and pass to a conveyor
      val targ_id = request.body.asFormUrlEncoded.get.get("target").get.head.toLong
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

  def itemUrl(id: Long) = {
    routes.Application.item(id)
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
        pm2.addMapping(scheme_id, source, format, 0)
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
    Ok(views.html.subscriber_index(Subscriber.all))
  }

  def subscriber(id: Long) = Action {
    Subscriber.findById(id).map( s => 
      Ok(views.html.subscriber(s))
    ).getOrElse(NotFound("No such subscriber"))
  }

  def subscriberBrowse(filter: String, value: String, page: Int) = Action {
    filter match {
      case "type" => subscriberBrowseType(value, page)
      case _ => NotFound("No such filter")
    }
  }

  def subscriberBrowseType(value: String, page: Int) = {
    Ok(views.html.subscriber_browse(value, Subscriber.inRole(value, page), value, page, Subscriber.roleCount(value)))
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
      "protocol" -> nonEmptyText,
      "load" -> nonEmptyText,
      "description" -> nonEmptyText,
      "userId" -> nonEmptyText,
      "password" -> nonEmptyText,
      "targetUrl" -> nonEmptyText,
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
        Target.create(sid, value.protocol, value.load, value.description, value.userId, value.password, value.targetUrl)
        Redirect(routes.Application.subscriber(sid))
      }
    )
  }

  def subscription(id: Long) = Action {
    Subscription.findById(id).map( s => {
      val topic = Topic.findById(s.topic_id).get
      Ok(views.html.subscription(s, topic))
      }
    ).getOrElse(NotFound("No such subscription"))
  }

  // Admin functions
  def reindex(dtype: String) = Action {
    indexer ! dtype
    Ok(views.html.index("reindex complete"))
  }

  // Content Model operations

  // export the content model in JSON
  def cmodel = Action { 
    val model = Map(
      "schemes" -> jsonSchemes,
      "ctypes" -> jsonCtypes,
      "pkgmaps" -> jsonPkgmaps
    )
    Ok(Json.stringify(toJson(model)))
  }

  def jsonSchemes = {
    val msg = Scheme.all.map( s => 
      Map("schemeId" -> toJson(s.schemeId),
          "gentype"  -> toJson(s.gentype),
          "category" -> toJson(s.category),
          "description" -> toJson(s.description),
          "home" -> toJson(s.home),
          "logo" -> toJson(s.logo),
          "finders" -> jsonFinders(s.id)
      )
    )
    toJson(msg)
  }

  def jsonFinders(sid: Long) = {
    val msg = Finder.findByScheme(sid).map ( f =>
      Map("description" -> toJson(f.description),
          "cardinality" -> toJson(f.cardinality),
          "format" -> toJson(f.format),
          "idKey" -> toJson(f.idKey),
          "idLabel" -> toJson(f.idLabel),
          "author" -> toJson(f.author)
      )
    )
    toJson(msg)
  }

  def jsonCtypes = {
    val msg = Ctype.all.map( t =>
      Map("ctypeId" -> toJson(t.ctypeId),
          "description" -> toJson(t.description),
          "logo" -> toJson(t.logo),
          "meta" -> toJson(t.schemes("meta").map(s => s.schemeId)),
          "index" -> toJson(t.schemes("index").map(s => s.schemeId)),
          "topic" -> toJson(t.schemes("topic").map(s => s.schemeId))
      )
    )
    toJson(msg)
  }

  def jsonPkgmaps = {
    val msg = PackageMap.all.map( m =>
      Map("pkgmapId" -> toJson(m.pkgmapId),
          "description" -> toJson(m.description),
          "swordurl" -> toJson(m.swordurl),
          "mappings" -> jsonPkgMappings(m)
      )
    )
    toJson(msg)
  }

  def jsonPkgMappings(pkgMap: PackageMap) = {
    val msg = pkgMap.schemes.map(s => 
      Map ("scheme" -> toJson(s.schemeId),
           "maps" -> jsonPkgSchemeMappings(pkgMap, s))
    )
    toJson(msg)
  }

  def jsonPkgSchemeMappings(pkgMap: PackageMap, scheme: Scheme) = {
    val msg = pkgMap.mappingsForScheme(scheme).map( m =>
      Map("source" -> toJson(m._1),
          "format" -> toJson(m._2),
          "rank" -> toJson(m._3)
      )
    ) 
    toJson(msg)   
  }

  def setCmodel = Action(parse.json) { request =>
    // read in a content model and update system in cases where 
    // model compnents are not already installed. Note that
    // this relies on the uniqueness of scheme, etc Ids
    val schemes = (request.body \ "schemes")
    procJsArray(schemes, 0, schemeFromCmodel)
    val ctypes = (request.body \ "ctypes")
    procJsArray(ctypes, 0, ctypeFromCmodel)
    val pkgmaps = (request.body \ "pkgmaps")
    procJsArray(pkgmaps, 0, pkgmapFromCmodel)
    Ok("???")
  }

  def procJsArray(arr: JsValue, index: Int, func: JsValue => Unit): Unit = {
    arr(index) match {
      case und: JsUndefined => Nil
      case jsval: JsValue => func(jsval); procJsArray(arr, index + 1, func)
    }
  }

  def schemeFromCmodel(jss: JsValue) {
    //println(Json.stringify(jss))
    val schemeId = forName(jss, "schemeId")
    // only create if not already defined
    if (Scheme.findByName(schemeId).isEmpty) {
      Scheme.create(schemeId, forName(jss, "gentype"), forName(jss, "category"),
                    forName(jss, "description"), oforName(jss, "home"), oforName(jss, "logo"))
      val scheme = Scheme.findByName(schemeId).get
      val finders = (jss \ "finders")
      procJsArray(finders, 0, finderFromCmodel(scheme.id))
    }
  }

  def finderFromCmodel(scheme_id: Long)(jsf: JsValue) {
    Finder.create(scheme_id, forName(jsf, "description"), forName(jsf, "cardinality"), forName(jsf, "format"),
                  forName(jsf, "idKey"), forName(jsf, "idLabel"), forName(jsf, "author"))
  }

  def ctypeFromCmodel(jsc: JsValue) {
    val ctypeId = forName(jsc, "ctypeId")
    // only create if not already defined
    if (Ctype.findByName(ctypeId).isEmpty) {
      Ctype.create(ctypeId, forName(jsc, "description"), oforName(jsc, "logo"))
      val ctype = Ctype.findByName(ctypeId).get
      procJsArray((jsc \ "meta"), 0, ctMapFromCmodel(ctype, "meta"))
      procJsArray((jsc \ "index"), 0, ctMapFromCmodel(ctype, "index"))
      procJsArray((jsc \ "topic"), 0, ctMapFromCmodel(ctype, "topic"))
    }
  }

  def ctMapFromCmodel(ctype: Ctype, reln: String)(jsc: JsValue) {
    val scheme = Scheme.findByName(jsc.as[String]).get
    ctype.addScheme(scheme, reln)
  }

  def pkgmapFromCmodel(jsp: JsValue) {
    val pkgmapId = forName(jsp, "pkgmapId")
    // only create if not already defined
    if (PackageMap.findByName(pkgmapId).isEmpty) {
      PackageMap.create(pkgmapId, forName(jsp, "description"), oforName(jsp, "swordurl"))
      val pkgmap = PackageMap.findByName(pkgmapId).get
      val mappings = (jsp \ "mappings")
      procJsArray(mappings, 0, pmMapFromCmodel(pkgmap))
    }
  }

  def pmMapFromCmodel(pkgmap: PackageMap)(jsc: JsValue) {
    val schemeId = forName(jsc, "scheme")
    val scheme = Scheme.findByName(schemeId).get
    val maps = (jsc \ "maps")
    procJsArray(maps, 0, pmSubMapFromCmodel(pkgmap, scheme.id))
  }

  def pmSubMapFromCmodel(pkgmap: PackageMap, sid: Long)(jsm: JsValue) {
    pkgmap.addMapping(sid, forName(jsm, "source"), forName(jsm, "format"), forNum(jsm, "rank"))
  }

  def forName(jsv: JsValue, name: String): String = (jsv \ name).as[String]
  def oforName(jsv: JsValue, name: String): Option[String] = (jsv \ name).asOpt[String]
  def forNum(jsv: JsValue, name: String): Int = (jsv \ name).as[Int]
}
