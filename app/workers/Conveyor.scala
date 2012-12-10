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

  val emailSvc = Play.configuration.getString("hub.email.url").get
  val cset = Charset.forName("UTF-8")
  val hubUrl = Play.configuration.getString("hub.server.url").get

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
        val transfer = Transfer.make(channel.id, item.id, sub.id, Some("none"))
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
        val transfer = Transfer.make(channel.id, item.id, sub.id, Some("none"))
        transferItem(transfer, item, channel, Some(topic))      
      })
    })
  }

  private def transferItem(transfer: Transfer, item: Item, channel: Channel, topic: Option[Topic]) {
    // only transfer options currently supported are SWORD deposit or email notification
    channel.protocol match {
      case "sword" => SwordClient.makeDeposit(item, channel)
      case "email" => emailNotify(item, channel, topic.get)
    } 
    // update objects to reflect delivery
    transfer.updateState("done")
    item.recordTransfer
    channel.recordTransfer
    if (transfer.subscription_id != -1) {
      Subscription.findById(transfer.subscription_id).get.recordTransfer
      topic.get.recordTransfer
    }
  }

  // this implementation is very mailgun-specific - ultimately will
  // need to provide a plugin system (including smtp option)
  def emailNotify(item: Item, channel: Channel, topic: Topic) {

    import java.io.ByteArrayOutputStream
    import org.apache.http.entity.mime.MultipartEntity
    import org.apache.http.entity.mime.content._

    val entity = new MultipartEntity()
    // should all customization of this
    entity.addPart("from", new StringBody("noreply@topichub.org", cset))
    // assumes targetUrl is a 'mailto:' URL (length 7)
    entity.addPart("to", new StringBody(channel.channelUrl.substring(7), cset))
    val subj = "TopicHub Alert - new in: " + topic.name
    entity.addPart("subject", new StringBody(subj, cset))
    val text = "Now available on TopicHub:\n" + 
               "Title: " + item.metadataValue("title") + "\n" +
               "Authors: " + item.metadataValues("author").mkString(";") + "\n" +
               "Link: " + hubUrl + Application.itemUrl(item.id)
    entity.addPart("text", new StringBody(text, cset))
    val out = new ByteArrayOutputStream
    entity.writeTo(out)
    //println("about to email: " + emailSvc)
    val header = (entity.getContentType.getName, entity.getContentType.getValue)
    val req = WS.url(emailSvc)
    .withHeaders(header)
    .withAuth(channel.userId, channel.password, AuthScheme.BASIC)
    val resp = req.post(out.toByteArray()).await.get
    //println("resp: " + resp.body)
  }
}
