package com.cascadeofinsights.scraper.models


import com.danielasfregola.twitter4s.entities.{RatedData, User => TwitterUser}

case class User(
                 name: String,
                 twitterHandle: String,
                 description: String,
                 follower_count: Int,
                 urls: List[URL],
                 home: Option[URL] = None,
                 email: Option[URL] = None) {

  def toTwitterName: TwitterName = TwitterName(twitterHandle.toLowerCase()).get

  def addURLs(urls : List[URL]): User = this.copy(urls = urls)

}

object User {
  def from(tu: TwitterUser): User = {
    User(tu.name, tu.screen_name, tu.description.getOrElse(""), tu.followers_count, List.empty, tu.url.flatMap(URL.apply(_)))
  }

  def update(users : Seq[User], urls : Map[TwitterName,Set[URL]]): Seq[User] = {
   val updated =  users.map(user => user.addURLs(urls.get(user.toTwitterName).getOrElse(Set.empty).toList))
    updated.sortBy(_.follower_count)
  }

}