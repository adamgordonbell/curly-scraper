package com.cascadeofinsights.scraper.models

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scalaz.Monad
import scalaz.zio.{ExitResult, IO, Promise}
import scalaz._
import Scalaz._
import com.cascadeofinsights.util.Cache
import scalaz.zio.interop.scalaz72._

object Gets {

  def getURLCached(rootPath : Path): URL => IO[Exception, String] =
    (url: URL) => {
      val cached = Cache.getFromDisk(rootPath)(url.digest)
      getOrElseEffect(cached,getURL(url))
    }.redeemPure[Nothing,String](
      err = (_ => ""), //cache any get errors as an empty string
      succ = identity
    )



  def getOrElseEffect[M[_]: Monad, A](a: M[Option[A]], b: => M[A]): M[A] =
    Monad[M].bind(a)(_.fold(b)(Monad[M].pure(_)))

  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()

  def getURL(url: URL): IO[Exception, String] = {
    for {
      //eventually this would be IO.blocking
      promise <- Promise.make[Exception, String]
      _ = println(s"getURL : $url")
      _ <- (for {
        exitResult <- getInThread(url)
        _ <- promise.done(exitResult)
      } yield ()).fork
      html <- promise.get
      _ = println(s"got : $url")
    } yield html
  }

  private def getInThread(url: URL): IO[Nothing, ExitResult[Exception, String]] = {
    IO.async[Nothing, ExitResult[Exception, String]](k => blockingPool.submit(
      new Runnable() {
        def run: Unit =
          try {
            k(ExitResult.Completed(ExitResult.Completed(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)))
          } catch {
            case e: Exception => k(ExitResult.Completed(ExitResult.Failed(e)))
          }
      }
    )): IO[Nothing, ExitResult[Exception, String]]
  }
}
