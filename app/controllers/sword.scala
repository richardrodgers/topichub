/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package controllers

import scala.xml.{NodeSeq, XML}

import akka.actor.Props

import java.io.{ByteArrayOutputStream, File, FileInputStream, FileOutputStream, InputStream}

import org.joda.time.format.ISODateTimeFormat

import org.apache.commons.codec.binary.Base64

import com.ning.http.client.Realm._
//import org.apache.http.entity.BasicHttpEntity
//import org.apache.http.client.methods.HttpPost
//import org.apache.http.impl.client.DefaultHttpClient

import play.api._
import play.api.libs.iteratee.Enumerator
import play.api.libs.ws.WS
import play.api.libs.Files.TemporaryFile
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.http.HeaderNames._
import play.api.http.Writeable
import play.api.Play.current

import models._
import store._
import workers.CatalogWorker

/** sword manages SWORD protocol operations for topicHub server,
  * which can act as a target (SWORD server) and as a client.
  * Current implementation supports only version 1.3
  *
  * @author richardrodgers
  */

object SwordServer extends Controller {

  val iso8601 = ISODateTimeFormat.dateTime

  val maxUploadSize = -1

  val cataloger = Akka.system.actorOf(Props[CatalogWorker], name="cataloger")

  /** Returns the SWORD service document for this server
    */
  def serviceDocument = Action { implicit request =>
    // return all collections for now - maybe filter based on requester later
    SimpleResult(
      header = ResponseHeader(200, Map(CONTENT_TYPE -> "application/atomsvc+xml; charset=\"utf-8\"")),
      body = Enumerator(serviceDoc(Collection.findAll, request.host))
    )
  }

  /** Validates and receives SWORD deposit
    */

  def processDeposit(id: Long) = Action(parse.temporaryFile) { implicit request =>
    Collection.findById(id).map( coll => {
      checkHeaders(request.headers).map ( error =>
        SimpleResult (
          header = ResponseHeader(error._1, Map(CONTENT_TYPE -> "application/atom+xml; charset=\"utf-8\"")),
          body = Enumerator(errorDoc(error._2.toString))
        )
      ).getOrElse( {
        val hdrMd5 = request.headers.get("content-md5").getOrElse(null)
        val item = uploadPackage(request.body, coll, hdrMd5)
        //if (item == null) println("uploadPackage failed")
        SimpleResult (
          header = ResponseHeader(201, Map(CONTENT_TYPE -> "application/atom+xml; charset=\"utf-8\"")),
          body = Enumerator(entryDoc(item))
        )
      })
    }).getOrElse(NotFound("No such collection"))
  }

  private def checkHeaders(headers: Headers): Option[(Int, SwordError.type)] = {
    val reportedLength = headers.get("content-length")
    if (reportedLength != null) {
      //println("Found CL")
      val repLen = reportedLength.get.toInt
      if (maxUploadSize > 0 && (repLen / 1024) > maxUploadSize) {
        (400, SwordError.MaxUploadSizeExceeded)
      } else {
        None
      }
    }
    None
  }

  private def uploadPackage(tempFile: TemporaryFile, coll: Collection, hdrMd5: String): Item = { 
  
    val item = Store.upload(tempFile, maxUploadSize, hdrMd5) match {
      case Right(it) => createItem(coll, it)
      case Left(err) => null //error = err; null
    }
    if (item != null) {
      // update all objects participating in the deposit - transfer, channel, and collection
      val chan = coll.channel.get
      Transfer.make(chan.id, item.id, 0L, None).updateState("done")
      chan.recordTransfer
      coll.recordDeposit
      // notify an asynch cataloger worker for further processing
      cataloger ! item
    }
    item
  }

  /** Validates and receives a SWORD deposit to this server
    */
    /*
  def acceptDeposit(collId: Long) = Action(parse.temporaryFile) { implicit request =>
    // do as much preliminary checking as we can before reading the package
    // e.g. content too big, unknown package URI or collection, etc
    println("Accepting deposit to: " + collId)
    var error: SwordError.Value = null
    val reportedLength = request.headers.get("content-length")
    if (reportedLength != null) {
      println("Found CL")
      val repLen = reportedLength.get.toInt
      if (maxUploadSize > 0 && (repLen / 1024) > maxUploadSize) {
        error = SwordError.MaxUploadSizeExceeded
      }
    }
    var coll: Collection = null
    if (error == null) {
      coll = Collection.findById(collId) match {
        case Some(x) => x
        case None => error = SwordError.UnsupportedPackage; null
      }
      val pkgType = request.headers.get("x-packaging")
      if (error == null && pkgType != null) {
        if (! coll.packaging.equals(pkgType.get)) {
          error = SwordError.UnsupportedPackage
        }
      }
    }
    val hdrMd5 = request.headers.get("content-md5").getOrElse(null)
    var item: Item = null
    if (error == null) {
      val tmpFile: File = File.createTempFile("sword-" + coll.id.toHexString, ".zip")
      request.body.moveTo(tmpFile)
      item = Store.upload(tmpFile, maxUploadSize, hdrMd5) match {
        case Right(it) => createItem(coll, it)
        case Left(err) => error = err; null
      }
      if (item != null) {
        // get rid of temp file
        tmpFile.delete
        // update all objects participating in the deposit - transfer, channel, and collection
        val chan = coll.channel.get
        Transfer.make(chan.id, item.id, 0L, None).updateState("done")
        chan.recordTransfer
        coll.recordDeposit
        // notify an asynch cataloger worker for further processing
        cataloger ! item
      }
    }
    SimpleResult (
      header =  ResponseHeader(200, Map(CONTENT_TYPE -> "application/atom+xml; charset=\"utf-8\"")),
      body = Enumerator(if (error == null) entryDoc(item) else errorDoc(error.toString))
    )
  }
  */

  private def createItem(coll: Collection, content: StoredContent): Item = {
    Item.findById(Item.create(coll.id, coll.ctype_id, content.md5).get).get
  }

  private def serviceDoc(colls: Seq[Collection], host: String) =
    <app:service xmlns:atom="http://www.w3.org/2005/Atom" 
                 xmlns:app="http://www.w3.org/2007/app" 
                 xmlns:sword="http://purl.org/net/sword/"
                 xmlns:dcterms="http://purl.org/dc/terms/">
      <sword:version>1.3</sword:version>
      <sword:maxUploadSize>{maxUploadSize}</sword:maxUploadSize>
      <atom:generator uri="http://www.dspace.org/ns/sword/1.3.1" version="1.3"/>
      <app:workspace>
        <atom:title type="text">TopicHub Deposits</atom:title>
        { for (coll <- colls) yield
          <app:collection href={"http://" + host + "/sword/collection/" + coll.id}>
            <atom:title type="text">{coll.description}</atom:title>
            <app:accept>application/zip</app:accept>
            <sword:acceptPackaging q="1.0">{coll.packaging}</sword:acceptPackaging>
            <sword:collectionPolicy>{coll.policy}</sword:collectionPolicy>
            <sword:mediation>true</sword:mediation>
          </app:collection>
        }
      </app:workspace>
    </app:service>

  private def entryDoc(item: Item) =
    <entry xmlns="http://www.w3.org/2005/Atom" xmlns:sword="http://purl.org/net/sword/">
      <sword:verboseDescription></sword:verboseDescription>
      <title>{item.metadataValue("title")}</title>
      <id>md5:{item.itemId}</id>
      <updated>{iso8601.print(item.created.getTime())}</updated>
      { for (auth <- item.metadataValues("author")) yield
        <author><name>{auth}</name></author>
      }
      <summary type="text">{item.metadataValues("author").mkString(",")} {item.metadataValue("title")}</summary>
      <content type="text/html"
        src="http://www.myrepository.ac.uk/fdibner/workbench/dummy_deposit"/>
      <link rel="edit-media"
        href="http://www.myrepository.ac.uk/geography/dummy_deposit.zip"/>
      <link rel="edit"
        href="http://www.myrepository.ac.uk/geography-collection/atom/dummy_deposit.atom" />
      <sword:userAgent>RepoGateway/1.1 PythonLibHttp2/2.5</sword:userAgent>
      <generator uri="http://www.myrepository.ac.uk/sword-plugin" version="1.0"/>
      <sword:treatment>Unpacked. JPEG contents converted to JPEG2000.</sword:treatment>
    </entry>

  private def errorDoc(errorUri: String) =
    <sword:error xmlns="http://www.w3.org/2005/Atom" xmlns:sword="http://purl.org/net/sword/" href={errorUri}>
      <title>Deposit Error</title>
    </sword:error>
}

object SwordClient {

  /**
   * Obtain the service document from SWORD server, and return parsed reply
   */
  def getServiceDoc(site: String, sub: Subscriber, channel: Channel): NodeSeq = {
    // try to construct a (DSpace) standard swordService URL if full URL not given
    val svcUrl = if (site.startsWith("http")) site else "http://" + site + "/sword/servicedocument"
   // Async {
      val resp = WS.url(svcUrl).withAuth(channel.userId, channel.password, AuthScheme.BASIC).get()
      resp.await.get.xml
      //resp map { response =>
        //SimpleResult (
        //  header = ResponseHeader(200, Map(CONTENT_TYPE -> "application/atom+xml; charset=\"utf-8\"")),
        //  body = Enumerator(response.xml)
       // response.xml
        //)
      //}
   // }
  }

  def makeDeposit(item: Item, channel: Channel) = {
    val cFile = Store.content(item)
    /* best way - but will not work since WS wants to set mime-type to multipart/form-data
         which 1.3 Sword server won't accept */
    var req = WS.url(channel.channelUrl)
    .withHeaders(CONTENT_TYPE -> "application/zip",
                 "X-packaging" -> "http://purl.org/net/sword-types/METSDSpaceSIP")
    .withAuth(channel.userId, channel.password, AuthScheme.BASIC)
    //req.setHeader("Content-Type", "application/zip")
    //req.setHeader("X-packaging", "http://purl.org/net/sword-types/METSDSpaceSIP")
    //req.setHeader("X-No-Op", "false")
    //req.authenticate(route.userId, route.password)
    //req.bod
    //req.files(new FileParam(depFile, depFile.getName))
    //req.setHeader("content-type", "application/zip")
    //req.mimeType("application/zip")
    //val foo = cFile.content.asInstanceOf[InputStream]
    // really lame - but apparently no stream-based way to do this in WS API
    val content = cFile.content
    val bytesOut = new ByteArrayOutputStream
    val buf = new Array[Byte](2048)
    var read = content.read(buf)
    while(read != -1) {
      bytesOut.write(buf, 0, read)
      read = content.read(buf)
    }
    //Async {
    var resp = req.post(bytesOut.toByteArray)
    //resp.await.get.xml
    //}
      // look up subscriber data
      //var sub = Subscription.find("byUserName", "rrodgers@mit.edu").first match {
      //  case Some(x) => x
      //  case _ => null
      //}

    // HttpClient way
    /*
    val client = new DefaultHttpClient
    val post = new HttpPost(route.depositUrl)
    post.addHeader("Content-Type", cFile.mimetype)
    post.addHeader("X-packaging", "http://purl.org/net/sword-types/METSDSpaceSIP")
    post.setHeader("Authorization", "Basic " + new String(Base64.encodeBase64((route.userId + ":" + route.password).getBytes)))
    val entity = new BasicHttpEntity
    entity.setContent(cFile.content)
    post.setEntity(entity)
    var response = client.execute(post)
    val statusCode = response.getStatusLine.getStatusCode
    println("Http status:" + statusCode)
    // return string indicating outcome
    if (statusCode == 200) {
      val retDoc = xml.XML.load(response.getEntity.getContent)
      if ((retDoc \ "error").size > 0) {
        "error"
      } else {
        "delivered"
      }
    } else {
      "fail"
    }
    */
  }
}

object SwordError extends Enumeration {
    // SWORD defined errors
    val ErrorContent = Value("http://purl.org/net/sword/error/ErrorContent")
    val ErrorChecksumMismatch = Value("http://purl.org/net/sword/error/ErrorChecksumMismatch")
    val ErrorBadRequest = Value("http://purl.org/net/sword/error/ErrorBadRequest")
    val TargetOwnerUnknown = Value("http://purl.org/net/sword/error/TargetOnwerUnknown")
    val MediationNotAllowed = Value("http://purl.org/net/sword/error/MediationNotAllowed")
    val MaxUploadSizeExceeded = Value("http://purl.org/net/sword/error/MaxUploadSizeExceeded")
    // extended error codes
    val UnsupportedPackage = Value("http://topichub.org/sword/error/UnsupportedPackage")
    val DuplicateContent = Value("http://topichub.org/sword/error/DuplicateContent")
}
