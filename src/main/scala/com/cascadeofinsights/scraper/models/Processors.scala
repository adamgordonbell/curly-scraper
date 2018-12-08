package com.cascadeofinsights.scraper.models

import java.io.{File, FileWriter}
import java.nio.file.Path

import com.cascadeofinsights.util.Cache
import scalaz.zio.IO

import scala.util.Try

object Processors {

  val Id: (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) => IO.now(List(url -> html))


  val TwitterNamesProcessor: (URL, String) => IO[Unit, List[(URL, List[TwitterName])]] =
    (url, html) => IO.now{
      List((url, TwitterName.extractNames(html)))
    }

  def returnAndCache(rootPath : Path) : (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) =>
      for {
        _ <- Cache.writeToDisk(rootPath)(url.digest, html)
        r <- Id(url,html)
      } yield r


  def cachedTwitter(rootPath : Path) : (URL, String) => IO[Unit, List[(URL, List[TwitterName])]] =
    (url, html) =>
      for {
        _ <- Cache.writeToDisk(rootPath)(url.digest, html)
        r <- TwitterNamesProcessor(url,html)
      } yield r

}


