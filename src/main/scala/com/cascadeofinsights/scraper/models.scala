package com.cascadeofinsights.scraper

import java.io.{File, FileWriter}
import java.nio.file.{Files, Path, Paths}

import net.degoes.scraper.test.SiteIndex
import net.degoes.scraper.url.URL
import scalaz.zio._
import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}

import com.cascadeofinsights.scraper.url.URL

import scala.util.Try

object models {

  def stayInSeedDomainRouter(seed : Set[URL]) : URL => Set[URL] =
    url => {
      val rootApexs = seed.map(_.parsed.apexDomain).flatten
      url.parsed.apexDomain.map(apex => if(rootApexs.contains(apex)) Set(url) else Set(): Set[URL]).getOrElse(Set())
  }

  def getURLCached(rootPath : Path): URL => IO[Exception, String] =
    (url: URL) => {
      val default = getURL(url)
      val cached = IO.sync{
        if(Files.exists(Paths.get(rootPath.toAbsolutePath.toString + "/" + url.digest))){
          Some(new String(Files.readAllBytes(Paths.get("file.txt")), UTF_8))
        } else {
          None
        }
      }
      for {
        c <- cached
        d <- default
      } yield c.getOrElse(d)
    }

  def writeToCacheProcessor(rootPath : Path): (URL, String) => IO[Unit, Unit] =
    (url, html) => {
      IO.sync {
        val writer = Try(new FileWriter(new File(rootPath.toAbsolutePath.toString + "/" + url.digest)))
        writer.map(w => {w.write(html); w}).recoverWith{case _ => writer}.map(_.close)
      }
    }


  val IdProcessor: (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) => IO.now(List(url -> html))

  def returnAndCache(rootPath : Path) : (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) =>
    for {
      _ <- writeToCacheProcessor(rootPath)(url, html)
      r <- IdProcessor(url,html)
    } yield r

  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()

  def getURL(url: URL): IO[Exception, String] =
    for {
      //eventually this would be IO.blocking
      promise <-  Promise.make[Exception, String]
      _       <-  (for {
        exitResult <- IO.async[Nothing, ExitResult[Exception, String]](k => blockingPool.submit(
          new Runnable () {
            def run: Unit =
              try {
                k(ExitResult.Completed(ExitResult.Completed(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)))
              } catch {
                case e : Exception => k(ExitResult.Completed(ExitResult.Failed(e)))
              }
          }
        )) : IO[Nothing, ExitResult[Exception, String]]
        _          <- promise.done(exitResult)
      } yield ()).fork
      html    <-  promise.get
    } yield html
 }

