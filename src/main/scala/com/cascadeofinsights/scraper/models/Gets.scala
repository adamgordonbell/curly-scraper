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
      for {
        c <- cached
        d <- getURL(url)
      } yield c.getOrElse(d)
    }

  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()

  def getURL(url: URL): IO[Exception, String] = {
    println(s"getURL : $url")
    for {
      //eventually this would be IO.blocking
      promise <- Promise.make[Exception, String]
      _ <- (for {
        exitResult <- IO.async[Nothing, ExitResult[Exception, String]](k => blockingPool.submit(
          new Runnable() {
            def run: Unit =
              try {
                k(ExitResult.Completed(ExitResult.Completed(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)))
              } catch {
                case e: Exception => k(ExitResult.Completed(ExitResult.Failed(e)))
              }
          }
        )): IO[Nothing, ExitResult[Exception, String]]
        _ <- promise.done(exitResult)
      } yield ()).fork
      html <- promise.get
    } yield html
  }
}
