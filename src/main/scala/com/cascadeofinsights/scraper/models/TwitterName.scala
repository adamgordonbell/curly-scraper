package com.cascadeofinsights.scraper.models

import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils

import scala.util.Try

final case class TwitterName private (name : String) {



 override def equals(a: Any): Boolean = a match {
  case that : TwitterName => this.name == that.name
  case _ => false
 }

 override def hashCode: Int = name.hashCode
}

 object TwitterName {


  def apply(name: String): Option[TwitterName] = {
    if (reserved.contains(name.toLowerCase)) {
      None
    } else {
      Some(new TwitterName(name.toLowerCase))
    }
  }

  def extractNames(html: String): List[TwitterName] = {
   val urlPattern = "(?<=twitter.com/)\\w*".r
    urlPattern.findAllMatchIn(html).toList.map(m => TwitterName(m.toString())).flatten
  }

   val reserved: Map[String, String] = List(
     "about",
     "account",
     "accounts",
     "activity",
     "all",
     "announcements",
     "anywhere",
     "api_rules",
     "api_terms",
     "apirules",
     "apps",
     "auth",
     "badges",
     "blog",
     "business",
     "buttons",
     "contacts",
     "devices",
     "direct_messages",
     "download",
     "downloads",
     "edit_announcements",
     "faq",
     "favorites",
     "find_sources",
     "find_users",
     "followers",
     "following",
     "friend_request",
     "friendrequest",
     "friends",
     "goodies",
     "help",
     "home",
     "im_account",
     "inbox",
     "invitations",
     "invite",
     "jobs",
     "list",
     "login",
     "logo",
     "logout",
     "me",
     "mentions",
     "messages",
     "mockview",
     "newtwitter",
     "notifications",
     "nudge",
     "oauth",
     "phoenix_search",
     "positions",
     "privacy",
     "public_timeline",
     "related_tweets",
     "replies",
     "retweeted_of_mine",
     "retweets",
     "retweets_by_others",
     "rules",
     "saved_searches",
     "search",
     "sent",
     "settings",
     "share",
     "signup",
     "signin",
     "similar_to",
     "statistics",
     "terms",
     "tos",
     "translate",
     "trends",
     "tweetbutton",
     "twttr",
     "update_discoverability",
     "users",
     "welcome",
     "who_to_follow",
     "widgets",
     "zendesk_auth",
     "media_signup").map(t => (t,t)).toMap
 }
