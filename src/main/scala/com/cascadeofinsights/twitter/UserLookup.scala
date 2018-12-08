package com.cascadeofinsights.twitter

import com.cascadeofinsights.scraper.models.TwitterName
import scalaz.zio.IO
import com.danielasfregola.twitter4s.TwitterRestClient
import scalaz._
import Scalaz._
import com.danielasfregola.twitter4s.entities.{RatedData, User}
import scalaz.zio.interop.future._
import scalaz.zio.IO

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._


object UserLookup {

  val restClient = TwitterRestClient()

  def lookupProfile(users: List[TwitterName]): IO[Nothing, Seq[(String, Int)]] = {
    IO.traverse(users.grouped(100).toList)(lookup100Max).map(_.flatten)
  }

  private def lookup100Max(users: List[TwitterName]): IO[Nothing, Seq[(String, Int)]] = {
    val users1 = users.take(100).map(_.name)
    val f: Future[RatedData[Seq[User]]] = restClient.users(users1)
    val io: IO[Throwable, RatedData[Seq[User]]] = IO.fromFuture { () => f }(ExecutionContext.global)
    val fr: IO[Throwable, Seq[(String, Int)]] = io.map(_.data.map(t => (t.name, t.followers_count)))
    fr.delay(1.seconds).attempt.map {
      case Left(a) => Seq.empty
      case Right(b) => b
    }
  }
}
