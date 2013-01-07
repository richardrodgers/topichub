/**
  * Copyright 2012 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import java.nio.charset.Charset
import akka.actor.Actor
import com.ning.http.client.Realm._
import play.api._
import play.api.Play.current
import play.api.http.HeaderNames._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.ws.WS

import controllers.{Application, SwordClient}
import models.{Channel, Item, Transfer, Subscription, Topic}
import models.HubUtils._

/** Conveyor delivers content or data to specified targets.
  * Currently only delivery mode is SWORD deposit
  * or email notifications (using REST API for email)
  *
  * @author richardrodgers
  */

class ConveyorWorker extends Actor {
  def receive = {
    case transfer: Transfer => Conveyor.transfer(transfer)
    case subscription: Subscription => Conveyor.fulfill(subscription)
    case item: Item => Conveyor.newItem(item)
    case _ => println("Unknown task")
  }
}

object  Conveyor {

  import play.api.Play.current
  import java.io.ByteArrayOutputStream
  import org.apache.http.entity.mime.MultipartEntity
  import org.apache.http.entity.mime.content._

  val emailSvc = Play.configuration.getString("hub.email.url").get
  val cset = Charset.forName("UTF-8")
  val hubUrl = Play.configuration.getString("hub.server.url").get
  val adminEmail = Play.configuration.getString("hub.admin.email").get
  val mgApiKey = Play.configuration.getString("hub.mailgun.apikey").get

  def transfer(trans: Transfer) {
    // deliver item according to transfer instructions
    val item = Item.findById(trans.item_id).get
    val channel = Channel.findById(trans.channel_id).get
    transferItem(trans, item, channel, None)
  }

  def fulfill(sub: Subscription) {
    // determine subscription policy: if retroactive, create
    // and execute transfers for all items in sub's topic
    if ("all".equals(sub.policy)) {
      val topic = Topic.findById(sub.topic_id).get
      val channel = Channel.findById(sub.channel_id).get
      topic.items.foreach( item => {
        val transfer = Transfer.make(channel.id, item.id, topic.id, Some("none"))
        transferItem(transfer, item, channel, Some(topic))
      })
    }
  }

  def newItem(item: Item) {
    // check all item's topics for subscriptions
    // if present, fuilfill them for this item
    item.topics.foreach( topic => {
      topic.subscriptions.foreach( sub => {
        val channel = Channel.findById(sub.channel_id).get
        val transfer = Transfer.make(channel.id, item.id, topic.id, Some("none"))
        transferItem(transfer, item, channel, Some(topic))      
      })
    })
  }

  private def transferItem(transfer: Transfer, item: Item, channel: Channel, topic: Option[Topic]) {
    // only transfer options currently supported are SWORD deposit or email notification
    channel.protocol match {
      case "sword" => SwordClient.makeDeposit(item, channel)
      case "email" => emailNotify(item, channel, topic)
    } 
    // update objects to reflect delivery
    transfer.updateState("done")
    item.recordTransfer
    channel.recordTransfer
    if (transfer.topic_id != -1) {
      Subscription.findByTopicAndChannel(transfer.topic_id, channel.id).head.recordTransfer
      topic.get.recordTransfer
    }
  }

  // this implementation is very mailgun-specific - ultimately will
  // need to provide a plugin system (including smtp option)
  def emailNotify(item: Item, channel: Channel, topic: Option[Topic]) {

    val entity = new MultipartEntity()
    // should all customization of this
    entity.addPart("from", new StringBody("noreply@topichub.org", cset))
    // assumes targetUrl is a 'mailto:' URL (length 7)
    entity.addPart("to", new StringBody(channel.channelUrl.substring(7), cset))
    val subj = if (! topic.isEmpty) "TopicHub Alert - new in: " + topic.get.name
               else "TopicHub Item Reminder"
    entity.addPart("subject", new StringBody(subj, cset))
    val text = "Now available on TopicHub:\n" + 
               "Title: " + item.metadataValue("title") + "\n" +
               "Authors: " + item.metadataValues("author").mkString(";") + "\n" +
               "Link: " + hubUrl + Application.itemUrl(item.id)
    entity.addPart("text", new StringBody(text, cset))
    val out = new ByteArrayOutputStream
    entity.writeTo(out)
    //println("about to email: " + emailSvc + " userId: " + interpolate(channel.userId) + " pwd: " + interpolate(channel.password))
    val header = (entity.getContentType.getName, entity.getContentType.getValue)
    val req = WS.url(emailSvc)
    .withHeaders(header)
    .withAuth(interpolate(channel.userId), interpolate(channel.password), AuthScheme.BASIC)
    val resp = req.post(out.toByteArray()).await.get
    //println("resp: " + resp.body)
  }

  // also directly wired into Mailgun API - need abstraction
  def emailFeedback(from: String, reply: Boolean, msg: String) {

    val entity = new MultipartEntity()
    entity.addPart("from", new StringBody(from, cset))
    entity.addPart("to", new StringBody(adminEmail, cset))
    if (reply) entity.addPart("h:Reply-To", new StringBody(from))
    entity.addPart("subject", new StringBody("TopicHub Feedback", cset))
    entity.addPart("text", new StringBody(msg, cset))
    val out = new ByteArrayOutputStream
    entity.writeTo(out)
    //println("about to email: " + emailSvc)
    val header = (entity.getContentType.getName, entity.getContentType.getValue)
    val req = WS.url(emailSvc)
    .withHeaders(header)
    .withAuth("api", mgApiKey, AuthScheme.BASIC)
    val resp = req.post(out.toByteArray()).await.get
  }
}
