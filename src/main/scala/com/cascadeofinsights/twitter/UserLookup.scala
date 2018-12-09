package com.cascadeofinsights.twitter

import java.nio.file.Path

import com.cascadeofinsights.scraper.models.TwitterName
import com.cascadeofinsights.util.Cache
import com.danielasfregola.twitter4s.TwitterRestClient
import com.danielasfregola.twitter4s.entities.{RatedData, User}
import org.json4s.{NoTypeHints, native}
import scalaz.Scalaz._
import scalaz.zio.interop.scalaz72._
import scalaz._
import scalaz.zio.IO
import scalaz.zio.console._
import scalaz.zio.interop.future._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


object UserLookup {

  val restClient = TwitterRestClient()

  def lookupProfile(users: List[TwitterName]): IO[Nothing, Seq[(String, Int)]] = {
    IO.traverse(users.grouped(100).toList)(lookup100Max).map(_.flatten)
  }

  def lookupProfileCached(rootPath : Path)(users: List[TwitterName]): IO[Nothing, Seq[(String, Int)]] = {
    IO.traverse(users.grouped(100).toList)(list => lookup100RawCached(rootPath)(list))
      .map(_.flatten)
      .map(_.map(t => (t.name,t.followers_count)))
  }

  private def lookup100Max(users: List[TwitterName]): IO[Nothing, Seq[(String, Int)]] = {
    lookup100MaxRaw(users).map(_.map(t => (t.name, t.followers_count)))
  }
  private def lookup100RawCached(rootPath : Path)(users: List[TwitterName]):IO[Nothing, Seq[User]] = {
    for{
      cached <- users.filterM(t => Cache.isOnDisk(rootPath)(t.digest))
      _ <- putStrLn(s"cached : $cached").attempt.void
      nonCached = (users.toSet -- cached).toList
      _ <- putStrLn(s"nonCached : $nonCached").attempt.void
      cachedResults <- cached.traverse(x => Cache.getFromDisk(rootPath)(x.digest))
          .map(_.flatten.map(toUser(_)).flatten)
      nonCachedResults <- lookup100MaxRaw(nonCached)
      _ <- nonCachedResults.toList.traverse(cache(rootPath)(_))
    } yield (nonCachedResults ++ cachedResults)
  }

  def cache(rootPath : Path)(user : User) : IO[Nothing,Unit] = {
    for {
      _ <- putStrLn(s"caching : ${user.screen_name.toLowerCase}").attempt.void
     _  <- Cache.writeToDisk(rootPath)(TwitterName(user.screen_name.toLowerCase).get.digest,fromUser(user))
    } yield ()
  }

  private def lookup100MaxRaw(users: List[TwitterName]): IO[Nothing, Seq[User]] = {
    if(users == List.empty) {
     IO.sync(Seq.empty)
    } else {
      val users1 = users.take(100).map(_.name)
      val f: Future[RatedData[Seq[User]]] = restClient.users(users1)
      val io: IO[Throwable, RatedData[Seq[User]]] = IO.fromFuture { () => f }(ExecutionContext.global)
      val fr: IO[Throwable, Seq[User]] = io.map(_.data)
      fr.delay(1.seconds).attempt.map {
        case Left(a) => Seq.empty
        case Right(b) => b
      }
    }
  }

  implicit val formats = native.Serialization.formats(NoTypeHints)

  def toUser(s : String) : Option[User] = Try(native.Serialization.read[User](s)).toOption
  def fromUser(u : User) : String = native.Serialization.write(u)
}
