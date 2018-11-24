package com.cascadeofinsights.scraper.devUtils

import java.nio.file.Paths

import com.cascadeofinsights.scraper.models._
import com.cascadeofinsights.scraper.Scraper
import scalaz.Scalaz._
import scalaz.zio.console._
import scalaz.zio.{App, IO}


object ScalazCrawl extends App {

  val rootFilePath = Paths.get("/Users/abell/temp1")
  val start = Set(
    URL("https://scalaz.github.io/7/").get
  )

  import replRTS._

  def firstPage() = {
    scraper.unsafeRun.value.head._2
  }
  val scraper: IO[Nothing, Crawl[Unit, List[(URL, String)]]] = Scraper.crawlIOPar(
    start,
    Routers.stayInSeedDomainRouter(start),
    Processors.returnAndCache(rootFilePath),
    Gets.getURLCached(rootFilePath)
  )

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Starting")
      rs <- scraper
      print = rs.value.map(_._1).mkString("\n")
      _ <- putStrLn(s"results : \n$print")
    } yield
      ()).redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )
}
