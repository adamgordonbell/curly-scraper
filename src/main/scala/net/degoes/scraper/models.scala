package net.degoes.scraper

import java.nio.file.Path

import net.degoes.scraper.test.SiteIndex
import net.degoes.scraper.url.URL
import scalaz.zio._

object models {

  def stayInSeedDomainRouter(seed : Set[URL]) : URL => Set[URL] =
    url => {
      val rootApexs = seed.map(_.parsed.apexDomain).flatten
      url.parsed.apexDomain.map(apex => if(rootApexs.contains(apex)) Set(url) else Set(): Set[URL]).getOrElse(Set())
  }

  def getURLCached(rootPath : Path): URL => IO[Exception, String] =
    (url: URL) => {
    //ToDo : check if exists in file system, if so return that

      ???
    }

  def writeToCacheProcessor(rootPath : Path): (URL, String) => IO[Unit, Unit] =
    //ToDo : make write to cache
    (url, html) => IO.now(())


  val IdProcessor: (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) => IO.now(List(url -> html))

  def returnAndCache(rootPath : Path) : (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) =>
    for {
      _ <- writeToCacheProcessor(rootPath)(url, html)
      r <- IdProcessor(url,html)
    } yield r
 }

