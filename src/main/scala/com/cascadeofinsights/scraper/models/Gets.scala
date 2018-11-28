package com.cascadeofinsights.scraper.models

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scalaz.zio.{ExitResult, IO, Promise}

object Gets {

  def getURLCached(rootPath : Path): URL => IO[Exception, String] =
    (url: URL) => {
      val cached = IO.sync{
        val path = Paths.get(rootPath.toAbsolutePath.toString + "/" + url.digest)
        if(Files.exists(path)){
          Some(new String(Files.readAllBytes(path), UTF_8))
        } else {
          None
        }
      }
      cached.flatMap(c => if(c.isDefined) IO.now(c.get) else getURL(url))
    }.redeemPure[Nothing,String](
      err = (_ => ""), //cache any get errors as an empty string
      succ = identity
    )

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
