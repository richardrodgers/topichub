/**
  * Copyright 2012 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import akka.actor.Actor

import controllers.SwordClient
import models.{Item, Target, Transfer, Subscription, Topic}

/** Conveyor delivers content to specified targets.
  * Currently only delivery mode is SWORD deposit
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

  def transfer(trans: Transfer) {
    // deliver item according to transfer instructions
    val item = Item.findById(trans.item_id).get
    val target = Target.findById(trans.target_id).get
    transferItem(trans, item, target)
  }

  def fulfill(sub: Subscription) {
    // determine subscription policy: if retroactive, create
    // and execute transfers for all items in sub's topic
    if ("all".equals(sub.policy)) {
      val topic = Topic.findById(sub.topic_id).get
      val target = Target.findById(sub.target_id).get
      topic.items.foreach( item => {
        val transfer = Transfer.make(target.id, item.id, sub.id, None)
        transferItem(transfer, item, target)
      })
    }
  }

  def newItem(item: Item) {
    // check all item's topics for subscriptions
    // if present, fuilfill them for this item
    item.topics.foreach( topic => {
      topic.subscriptions.foreach( sub => {
        val target = Target.findById(sub.target_id).get
        val transfer = Transfer.make(target.id, item.id, sub.id, None)
        transferItem(transfer, item, target)      
      })
    })
  }

  private def transferItem(transfer: Transfer, item: Item, target: Target) {
    // only SWORD deposit currently supported
    SwordClient.makeDeposit(item, target)
    // update objects to reflect delivery
    transfer.updateState("done")
    item.recordTransfer
    target.recordTransfer
    if (transfer.subscription_id != -1) {
      Subscription.findById(transfer.subscription_id).get.recordTransfer
    }
  }
}
