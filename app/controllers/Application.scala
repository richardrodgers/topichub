/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package controllers

import java.util.{Date}
import java.net.URLEncoder

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
import workers.{Cataloger, Conveyor, ConveyorWorker, IndexWorker}
import AuthN._

object Application extends Controller with Secured {

  val indexSvc = Play.configuration.getString("hub.index.url").get + "/topichub/"

  val conveyor = Akka.system.actorOf(Props[ConveyorWorker], name="conveyor")
  val indexer = Akka.system.actorOf(Props[IndexWorker], name="indexer")
  
  def index = Action { implicit request =>
    Ok(views.html.index(""))
  }

  def explain = Action { implicit request =>
    Ok(views.html.explain())
  }

  def about = Action { implicit request =>
    Ok(views.html.about())
  }

  def terms = Action { implicit request =>
    Ok(views.html.terms())
  }

  def workbench = Action { implicit request =>
    Ok(views.html.workbench())
  }

  val feedbackForm = Form(
    tuple(
      "email" -> email,
      "reply"  -> checked("what"),
      "comment" -> nonEmptyText
    ) verifying("Invalid Email address", result => true)
  )

  def feedback = Action { implicit request =>
    Ok(views.html.feedback(feedbackForm))
  }

  def takeFeedback = Action { implicit request =>
    feedbackForm.bindFromRequest.fold(
      errors => BadRequest(views.html.feedback(errors)),
      value => {
        //val user = User.findByName(session.get("username").get).get
        Conveyor.emailFeedback(value._1, value._2, value._3)
        Redirect(routes.Application.index)
      }
    )
  }

  def search = Action { implicit request =>
    val rquery = request.queryString
    val pageSize = 20
    val page = rquery.get("page").getOrElse(List("0")).head.toInt
    val offset = page * pageSize
    val etype = rquery.get("etype").get.head
    val query = rquery.get("query").getOrElse(List("foo")).head
    val encQuery = URLEncoder.encode(query)
    Async {
      val req = WS.url(indexSvc +  etype + "/_search?q=" + encQuery + "&from=" + offset + "&size=" + pageSize)
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
        } else if ("item".equals(etype)) {
          val items = hits map ( id => Item.findById(id) )
          Ok(views.html.item_search(items, query, page))
        } else if ("subscriber".equals(etype)) {
          val subs = hits map ( id => Subscriber.findById(id) )
          Ok(views.html.subscriber_search(subs, query, page))
        } else {
          NotFound(views.html.trouble("Unknown Search Entity Type"))
        }
      }
    }
  }

  def item(id: Long) = Action { implicit request =>
    Item.findById(id).map( item => 
      Ok(views.html.item(item))
    ).getOrElse(NotFound(views.html.trouble("No such item")))
  }

  def itemBrowse(filter: String, id: Long, page: Int) = Action { implicit request =>
    filter match {
      case "collection" => itemBrowseCollection(id, page)
      case "topic" => itemBrowseTopic(id, page)
      case _ => NotFound(views.html.trouble("No such filter"))
    }
  }

  private def itemBrowseTopic(id: Long, page: Int)(implicit request: Request[AnyContent]): Result = {
    Topic.findById(id).map( topic => 
      Ok(views.html.item_browse(id, topic.pagedItems(page, 10), "topic", topic.name, page, topic.itemCount))
    ).getOrElse(NotFound(views.html.trouble("No such topic")))
  }

  private def itemBrowseCollection(id: Long, page: Int)(implicit request: Request[AnyContent]): Result = { 
    Collection.findById(id).map( coll => 
      Ok(views.html.item_browse(id, Item.inCollection(id, page), "collection", coll.description, page, Item.collectionCount(coll.id)))
    ).getOrElse(NotFound(views.html.trouble("No such collection")))
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

  def itemFile(id: Long) = Action { implicit request =>
    Item.findById(id).map( item => {
      val itemPkg = Store.content(item)
      SimpleResult(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> itemPkg.mimetype)),
        body = Enumerator.fromStream(itemPkg.content)
      )
    }).getOrElse(NotFound(views.html.trouble("No such item")))  
  }

  def itemView(id: Long) = Action { implicit request =>
    Item.findById(id).map( i => {
      val (name, mimeType) = Cataloger.contentInfo(i)
      SimpleResult (
        header =  ResponseHeader(200, Map(CONTENT_TYPE -> mimeType)),
        body = Enumerator.fromStream(Store.content(i).resource(name))
      )
    }
    ).getOrElse(NotFound(views.html.trouble("No such item")))
  }

  def itemTransfer(id: Long, mode: String) = mustAuthenticate { username => implicit request =>
    Item.findById(id).map( item => {
      val msg = if ("package".equals(mode)) "Deposit" else "Notify"
      showTransferIfSubscriber(User.findByName(username).get, id, msg, mode)
    }
    ).getOrElse(NotFound(views.html.trouble("No such item")))
  }

  private def showTransferIfSubscriber(user: User, id: Long, msg: String, mode: String)(implicit request: Request[AnyContent]): Result = {
    Subscriber.findByUserId(user.id).map( sub =>
      Ok(views.html.item_transfer(id, sub.channelsWith(mode), msg)) 
    ).getOrElse(Redirect(routes.Application.subscribers))
  }

  def transfer(id: Long) = isAuthenticated { username => implicit request =>
    Item.findById(id).map( item => 
      transferIfSubscriber(User.findByName(username).get, item,
                           request.queryString.get("channel").getOrElse(List("0L")).head.toLong)
    ).getOrElse(NotFound(views.html.trouble("No such item")))
  }

  private def transferIfSubscriber(user: User, item: Item, channel_id: Long): Result = {
    Subscriber.findByUserId(user.id).map( sub => {
        // create a transfer and pass to a conveyor
        conveyor ! Transfer.make(channel_id, item.id, -1L, Some("unknown"))
        Redirect(routes.Application.item(item.id))      
      }
    ).getOrElse(Redirect(routes.Application.subscribers))
  }

  def topics = Action { implicit request =>
    Ok(views.html.topic_index(Scheme.withGentype("topic").filter(!_.schemeId.equals("meta"))))
  }

  def topic(id: Long) = Action { implicit request =>
    Topic.findById(id).map( t => 
      Ok(views.html.topic(t))
    ).getOrElse(NotFound(views.html.trouble("No such topic")))
  }

  def topicBrowse(scheme_id: Long, page: Int) = Action { implicit request =>
    Scheme.findById(scheme_id).map( scheme => 
      Ok(views.html.topic_browse(scheme.id, Topic.withScheme(scheme.id, page), scheme.description, page, scheme.topicCount))
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
  }

 def topicSubscribe(id: Long, mode: String) = mustAuthenticate { username => implicit request =>
    Topic.findById(id).map( topic => 
      topicSubIfSubscriber(User.findByName(username).get, topic, mode) 
    ).getOrElse(NotFound(views.html.trouble("No such topic")))
  }

  private def topicSubIfSubscriber(user: User, topic: Topic, mode: String)(implicit request: Request[AnyContent]): Result = {
    Subscriber.findByUserId(user.id).map( sub =>
      Ok(views.html.topic_subscribe(topic, sub.channelsWith(mode)))
    ).getOrElse(Redirect(routes.Application.subscribers))
  }

  def topicValidate(scheme_id: Long) = isAuthenticated { username => implicit request => 
    Scheme.findById(scheme_id).map( scheme => {
       val topic_id = request.body.asFormUrlEncoded.get.get("topic").get.head
       checkTopic(scheme, topic_id, username)
    }
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
  }

  private def checkTopic(scheme: Scheme, topicId: String, userName: String)(implicit request: Request[AnyContent]): Result = {
    val topic = Topic.forSchemeAndId(scheme.schemeId, topicId)
    if (! topic.isEmpty)
      // just redirect to topic page
      Redirect(routes.Application.topic(topic.get.id))
    else {
      // not a known topic in this scheme - attempt validation  
      val validator = scheme.validator
      val res = if (! validator.isEmpty) validator.get.validate(topicId) else Left("No validator found")
      Ok(views.html.topic_validate(scheme, topicId, res)) 
    }
  }

  def topicPresub(sid: Long, topicId: String, name: String, mode: String) = isAuthenticated { username => implicit request =>
    Scheme.findById(sid).map( scheme => 
      topicPreSubIfSubscriber(User.findByName(username).get, scheme, topicId, name, mode)
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
   // val topic = Topic.make(sid, topicId, name)
   // Redirect(routes.Application.subscribe(topic.id))
  }

  private def topicPreSubIfSubscriber(user: User, scheme: Scheme, topicId: String, name: String, mode: String)(implicit request: Request[AnyContent]): Result = {
    Subscriber.findByUserId(user.id).map( sub =>
      Ok(views.html.topic_presub(scheme, topicId, name, sub.channelsWith(mode)))
    ).getOrElse(Redirect(routes.Application.subscribers))
  }

  def subscribe(id: Long) = isAuthenticated { username => implicit request =>
    Topic.findById(id).map( t => {
      val user = User.findByName(username).get
      val sub = Subscriber.findByUserId(user.id).get
      // create a subscription and pass to a conveyor
      val channel_id = request.body.asFormUrlEncoded.get.get("channel").get.head.toLong
      val policy = request.body.asFormUrlEncoded.get.get("policy").get.head
      conveyor ! Subscription.make(sub.id, channel_id, id, policy)
      Redirect(routes.Application.topic(id))
    }
    ).getOrElse(NotFound(views.html.trouble("No such topic")))
  }

  def presubscribe(sid: Long, topicId: String, topicName: String) = isAuthenticated { username => implicit request =>
    Scheme.findById(sid).map( scheme => {
      val user = User.findByName(username).get
      val sub = Subscriber.findByUserId(user.id).get
      // create a subscription and pass to a conveyor
      val channel_id = request.body.asFormUrlEncoded.get.get("channel").get.head.toLong
      val policy = request.body.asFormUrlEncoded.get.get("policy").get.head
      val topic = Topic.make(sid, topicId, topicName)
      // debateable whether to index an unmanifested topic
      indexer ! topic
      conveyor ! Subscription.make(sub.id, channel_id, topic.id, policy)
      Redirect(routes.Application.topic(topic.id))
    }
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
  }

  /* Publisher methods */

  val pubForm = Form(
  	mapping(
      "id" -> ignored(0L),
      "userId" -> ignored(0L),
      "pubId" -> nonEmptyText,
      "name" -> nonEmptyText,
      "description" -> nonEmptyText,
      "category" -> nonEmptyText,
      "status" -> ignored(""),
      "link" -> optional(text),
      "logo" -> optional(text),
      "created" -> ignored(new Date)
    )(Publisher.apply)(Publisher.unapply)
  )

  private def ownsPublisher(username: String, pub: Publisher, result: Result)(implicit request: Request[AnyContent]): Result = {
    val user = User.findByName(username).get
    if (user.hasPublisher(pub.id)) {
      result
    } else {
      Unauthorized(views.html.trouble("You are not authorized"))
    }
  }

  def publishers = Action { implicit request => 
  	Ok(views.html.publisher_index())
  }

  def newPublisher = mustAuthenticate { username => implicit request =>
    Ok(views.html.new_publisher(null, pubForm))
  }

  def publisher(id: Long) = Action { implicit request => {
    val userName = session.get("username").getOrElse("")
    Publisher.findById(id).map( pub => 
      Ok(views.html.publisher(pub, User.findByName(userName)))
    ).getOrElse(NotFound(views.html.trouble("No such publisher")))
    }
  }

  def editPublisher(id: Long) = isAuthenticated { username => implicit request =>
    Publisher.findById(id).map( pub => 
      ownsPublisher(username, pub, Ok(views.html.publisher_edit(pub)))
    ).getOrElse(NotFound(views.html.trouble("No such publisher")))
  }

  def createPublisher = isAuthenticated { username => implicit request =>
    pubForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_publisher(null, errors)),
      value => {
        val user = User.findByName(username).get
        val pub = Publisher.make(user.id, value.pubId, value.name, value.description, value.category, value.status, value.link, value.logo)
        Redirect(routes.Application.editPublisher(pub.id))
      }
    )
  }

  def publisherBrowse(filter: String, value: String, page: Int) = Action { implicit request =>
    filter match {
      case "category" => Ok(views.html.publisher_browse(value, Publisher.inCategory(value, page), value, page, Publisher.categoryCount(value))) //publisherBrowseCategory(value, page)
      case _ => NotFound(views.html.trouble("No such filter"))
    }
  }

/*
  def publisherBrowseCategory(value: String, page: Int) = {
    Ok(views.html.publisher_browse(value, Publisher.inCategory(value, page), value, page, Publisher.categoryCount(value)))
  }
*/

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

  def newCollection(id: Long) = isAuthenticated { username => implicit request =>
    Publisher.findById(id).map( pub => 
      ownsPublisher(username, pub, Ok(views.html.new_collection(pub, collForm)))
    ).getOrElse(NotFound(views.html.trouble("No such publisher")))
  }

  def createCollection(id: Long) = isAuthenticated { username => implicit request =>
    val pub = Publisher.findById(id).get 
    ownsPublisher(username, pub, 
      collForm.bindFromRequest.fold(
        errors => BadRequest(views.html.new_collection(pub, errors)),
        value => {
          val coll = Collection.make(id, value.ctype_id, value.pkgmap_id, value.description, value.policy)
          // also create an inbound channel for this collection - currently limited to SWORD
          val chan = Channel.make("sword", "package", "inbound", pub.pubId + ":" + coll.description + " deposits", "user", "password", "/sword/collection/" + coll.id)
          // make collection the channel owner
          chan.setOwner("coll", coll.id)
          conveyor ! coll
          Redirect(routes.Application.publisher(id))
        }
      )
    )
    //).getOrElse(NotFound("No such publisher"))
  }

  def itemUrl(id: Long) = {
    routes.Application.item(id)
  }

  def isAnalyst(username: String, result: Result)(implicit hubContext: HubContext): Result = {
    val user = User.findByName(username).get
    if (user.role.indexOf("analyst") >= 0 || user.role.indexOf("admin") >= 0) {
      result
    } else {
      Unauthorized(views.html.trouble("You are not authorized"))
    }
  }

  /* Scheme methods */

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

  def schemes = isAuthenticated { username => implicit request =>
    isAnalyst(username, Ok(views.html.scheme_list(Scheme.all)))
  }

  def scheme(id: Long) = Action { implicit request => 
    Scheme.findById(id).map( scheme => 
      Ok(views.html.scheme(scheme))
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
  }

  def newScheme = isAuthenticated { username => implicit request =>
     isAnalyst(username, Ok(views.html.new_scheme(schemeForm)))
  }

  def editScheme(id: Long) = isAuthenticated { username => implicit request =>
    Scheme.findById(id).map( scheme => 
      isAnalyst(username, Ok(views.html.scheme_edit(scheme)))
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
  }

  def createScheme = isAuthenticated { username => implicit request =>
    isAnalyst(username,
      schemeForm.bindFromRequest.fold(
        errors => BadRequest(views.html.new_scheme(errors)),
        value => {
          Scheme.create(value.schemeId, value.gentype, value.category, value.description, value.link, value.logo)
          Redirect(routes.Application.schemes)
        }
      )
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

  def ctypes = isAuthenticated { username => implicit request =>
    isAnalyst(username, Ok(views.html.ctype_list(Ctype.all)))
  }

  def ctype(id: Long) = isAuthenticated { username => implicit request =>
    Ctype.findById(id).map( ctype => 
      isAnalyst(username, Ok(views.html.ctype(ctype, ctSchemeForm)))
    ).getOrElse(NotFound(views.html.trouble("No such content type")))
  }

  def newCtype = isAuthenticated { username => implicit request =>
     isAnalyst(username, Ok(views.html.new_ctype(ctypeForm)))
  }

  def createCtype = isAuthenticated { username => implicit request =>
    isAnalyst(username,
      ctypeForm.bindFromRequest.fold(
        errors => BadRequest(views.html.new_ctype(errors)),
        value => {
          Ctype.create(value.ctypeId, value.description, value.logo)
          Redirect(routes.Application.ctypes)
        }
      )
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

  def pkgmaps = isAuthenticated { username => implicit request =>
    isAnalyst(username, Ok(views.html.pkgmap_list(PackageMap.all)))
  }

  def pkgmap(id: Long) = isAuthenticated { username => implicit request =>
    PackageMap.findById(id).map( pmap => 
      isAnalyst(username, Ok(views.html.pkgmap(pmap, pmSchemeForm)))
    ).getOrElse(NotFound(views.html.trouble("No such package map")))
  }

  def newPkgmap = isAuthenticated { username => implicit request =>
     isAnalyst(username, Ok(views.html.new_pkgmap(pkgmapForm)))
  }

  def createPkgmap = isAuthenticated { username => implicit request =>
    isAnalyst(username, pkgmapForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_pkgmap(errors)),
      value => {
        PackageMap.create(value.pkgmapId, value.description, value.swordUrl)
        Redirect(routes.Application.pkgmaps)
      }
    ))
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

  def finders(schemeId: String) = Action { implicit request =>
    Scheme.findByName(schemeId).map( scheme =>
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme")))
  }

  def newFinder(schemeId: String) = isAuthenticated { username => implicit request =>
    Scheme.findByName(schemeId).map( scheme =>
      isAnalyst(username, Ok(views.html.new_finder(schemeId, finderForm)))
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme")))
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
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme")))
  }

  def deleteFinder(schemeId: String, id: Long) = Action { implicit request =>
    Scheme.findByName(schemeId).map( scheme => {
      Finder.delete(id)
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    }
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme")))
  }

  val finderTestForm = Form(
    tuple(
      "itemId" -> longNumber,
      "source" -> text,
      "idKey" -> text,
      "idLabel" -> text
    )
  )

  def testFinder = Action { implicit request =>
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

  val validatorForm = Form(
    mapping(
      "id" -> ignored(0L),
      "scheme_id" -> longNumber,
      "description" -> nonEmptyText,
      "userId" -> nonEmptyText,
      "password" -> nonEmptyText,
      "serviceCode" -> nonEmptyText,
      "serviceUrl" -> nonEmptyText,
      "author" -> nonEmptyText,
      "created" -> ignored(new Date)
    )(Validator.apply)(Validator.unapply)
  )

  def newValidator(schemeId: String) = isAuthenticated { username => implicit request =>
    Scheme.findByName(schemeId).map( scheme =>
      isAnalyst(username, Ok(views.html.new_validator(schemeId, validatorForm)))
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme")))
  }

  def createValidator(schemeId: String) = isAuthenticated { username => implicit request =>
    Scheme.findByName(schemeId).map( scheme =>
      validatorForm.bindFromRequest.fold(
        errors => BadRequest(views.html.new_validator(schemeId, errors)),
        value => {
          Validator.create(scheme.id, value.description, value.userId, value.password, value.serviceCode, value.serviceUrl, value.author)
          Redirect(routes.Application.finders(schemeId))
        }
      )
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme")))
  }

  def deleteValidator(schemeId: String, id: Long) = isAuthenticated { username => implicit request =>
    Scheme.findByName(schemeId).map( scheme => {
      Validator.delete(id)
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    }
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme")))
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

  /* Subscriber methods */

  val subscriberForm = Form(
    mapping(
      "id" -> ignored(0L),
      "user_id" -> ignored(0L),
      "category" -> nonEmptyText,
      "name" -> nonEmptyText,
      "visibility" -> nonEmptyText,
      "keywords" -> nonEmptyText,
      "link" -> optional(text),
      "logo" -> optional(text),
      "contact" -> nonEmptyText,
      "swordService" -> nonEmptyText,
      "terms" -> nonEmptyText,
      "backFile" -> ignored("yes"),
      "created" -> ignored(new Date)
    )(Subscriber.apply)(Subscriber.unapply)
  )

  private def ownsSubscriber(username: String, subId: Long, result: Result)(implicit request: Request[AnyContent]): Result = {
    val user = User.findByName(username).get
    if (user.hasSubscriber(subId)) {
      result
    } else {
      Unauthorized(views.html.trouble("You are not authorized"))
    }
  }

  def subscribers = Action { implicit request => 
    Ok(views.html.subscriber_index())
  }

  def subscriber(id: Long) = Action { implicit request => {
    Subscriber.findById(id).map( sub => 
      Ok(views.html.subscriber(sub))
    ).getOrElse(NotFound(views.html.trouble("No such subscriber")))
    }
  }

  def subscriberBrowse(filter: String, value: String, page: Int) = Action { implicit request =>
    filter match {
      case "category" => Ok(views.html.subscriber_browse(value, Subscriber.inCategory(value, page), value, page, Subscriber.categoryCount(value))) //subscriberBrowseCategory(value, page)
      case _ => NotFound(views.html.trouble("No such filter"))
    }
  }

  /*
  def subscriberBrowseCategory(value: String, page: Int) = {
    Ok(views.html.subscriber_browse(value, Subscriber.inCategory(value, page), value, page, Subscriber.categoryCount(value)))
  }
  */

  def newSubscriber = mustAuthenticate { username => implicit request =>
    Ok(views.html.new_subscriber(subscriberForm))
  }

  def editSubscriber(id: Long) = isAuthenticated { username => implicit request =>
    Subscriber.findById(id).map( sub => 
      ownsSubscriber(username, sub.id, Ok(views.html.subscriber_edit(sub)))
    ).getOrElse(NotFound(views.html.trouble("No such subscriber")))
  }

  def createSubscriber = isAuthenticated { username => implicit request =>
    subscriberForm.bindFromRequest.fold(
      errors => BadRequest(views.html.new_subscriber(errors)),
      value => {
        val user = User.findByName(username).get
        val sub = Subscriber.make(user.id, value.category, value.name, value.visibility, value.keywords, value.link, value.logo, value.contact, value.swordService, value.terms, value.backFile)
        // index values
        indexer ! sub
        Redirect(routes.Application.subscribers)
      }
    )
  }

  val channelForm = Form(
    mapping(
      "id" -> ignored(0L),
      "protocol" -> nonEmptyText,
      "mode" -> nonEmptyText,
      "direction" -> ignored("outbound"),
      "description" -> nonEmptyText,
      "userId" -> nonEmptyText,
      "password" -> nonEmptyText,
      "channelUrl" -> nonEmptyText,
      "created" -> ignored(new Date),
      "updated" -> ignored(new Date),
      "transfers" -> ignored(0)
    )(Channel.apply)(Channel.unapply)
  )

  def channel(id: Long) = isAuthenticated { username => implicit request =>
    Channel.findById(id).map( chan => 
      Ok(views.html.channel(chan))
    ).getOrElse(NotFound(views.html.trouble("No such subscriber destination")))
  }

  def newChannel(sid: Long) = isAuthenticated { username => implicit request =>
    ownsSubscriber(username, sid, Ok(views.html.new_channel(sid, channelForm)))
  }

  def createChannel(sid: Long) = isAuthenticated { username => implicit request =>
    ownsSubscriber(username, sid, channelForm.bindFromRequest.fold (
      errors => BadRequest(views.html.new_channel(sid, errors)),
      value => {
        val chan = Channel.make(value.protocol, value.mode, "outbound", value.description, value.userId, value.password, value.channelUrl)
        // make subscriber the channel owner
        chan.setOwner("sub", sid)
        Redirect(routes.Application.subscriber(sid))
      }
      )
    )
  }

  def cancelSubscription(id: Long) = isAuthenticated { username => implicit request =>
    Subscription.findById(id).map( sub => {
      val topic = Topic.findById(sub.topic_id).get
      Subscription.delete(id)
      // if topic is now an orphan (no items or other subscriptions), remove it
      if (topic.itemCount == 0 && topic.subscriptionCount == 0 && topic.transfers == 0) Topic.delete(topic.id)
      Redirect(routes.Application.channel(sub.channel_id))
    }).getOrElse(NotFound(views.html.trouble("No such subscription")))
  }

  // Admin functions
  def reindex(dtype: String) = isAuthenticated { username => implicit request =>
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
          "link" -> toJson(s.link),
          "logo" -> toJson(s.logo),
          "finders" -> jsonFinders(s.id),
          "validators" -> jsonValidators(s.id)
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

  def jsonValidators(sid: Long) = {
    val msg = Validator.findByScheme(sid).map ( v =>
      Map("description" -> toJson(v.description),
          "userId" -> toJson(v.userId),
          "password" -> toJson(v.password),
          "serviceCode" -> toJson(v.serviceCode),
          "serviceUrl" -> toJson(v.serviceUrl),
          "author" -> toJson(v.author)
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
          "swordUrl" -> toJson(m.swordUrl),
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
                    forName(jss, "description"), oforName(jss, "link"), oforName(jss, "logo"))
      val scheme = Scheme.findByName(schemeId).get
      val finders = (jss \ "finders")
      procJsArray(finders, 0, finderFromCmodel(scheme.id))
      val validators = (jss \ "validators")
      procJsArray(validators, 0, validatorFromCmodel(scheme.id))
    }
  }

  def finderFromCmodel(scheme_id: Long)(jsf: JsValue) {
    Finder.create(scheme_id, forName(jsf, "description"), forName(jsf, "cardinality"), forName(jsf, "format"),
                  forName(jsf, "idKey"), forName(jsf, "idLabel"), forName(jsf, "author"))
  }

  def validatorFromCmodel(scheme_id: Long)(jsf: JsValue) {
    Validator.create(scheme_id, forName(jsf, "description"), forName(jsf, "userId"), forName(jsf, "password"),
                  forName(jsf, "serviceCode"), forName(jsf, "serviceUrl"), forName(jsf, "author"))
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
      PackageMap.create(pkgmapId, forName(jsp, "description"), oforName(jsp, "swordUrl"))
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
