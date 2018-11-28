package com.cascadeofinsights.scraper.devUtils

import java.nio.file.Paths

import com.cascadeofinsights.scraper.Scraper
import com.cascadeofinsights.scraper.models._
import scalaz.Scalaz._
import scalaz.zio.console._
import scalaz.zio.{App, IO}

object ScaleByTheBayCrawl extends App {

  val rootFilePath = Paths.get("/Users/abell/temp1")
  val start = Set(
    URL("https://scalebythebay2018.sched.com/").get,
    URL("http://scale.bythebay.io/").get
  )

  import replRTS._

  val scraper: IO[Nothing, Crawl[Unit, List[(URL, List[TwitterName])]]] = Scraper.crawlIOPar(
    start,
    Routers.compose(
      Routers.stayInSeedDomainRouter(start),
      Routers.dropAnchorsAndQueryParams,
    ),
    Processors.cachedTwitter(rootFilePath),
    Gets.getURLCached(rootFilePath)
  )

  def correlate(values : List[(URL,List[TwitterName])]) : List[TwitterName] =
    values.map(_._2).flatten.distinct

  def correlate2(values : List[(URL,List[TwitterName])]) : Map[TwitterName,URL] = {
   values.map(tuple => tuple._2.map(twitter => (twitter, tuple._1))).flatten.toMap
  }

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Starting")
      rs <- scraper
      print = correlate2(rs.value).mkString("\n")
      _ <- putStrLn(s"results : \n$print")
    } yield
      ()).redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )


//  val results: Crawl[Unit, List[(URL, String)]] = scraper.unsafeRun
//  val firstPage: String = scraper.unsafeRun.value.head._2
//  val urls: List[URL] = URL.extractURLs(URL("https://scalaz.github.io/7/").get,ScalazCrawl.firstPage)
//  val urlsCleaned: List[URL] = urls.flatMap(u => Routers.stayInSeedDomainRouter(Set(start.head))(u))
}
