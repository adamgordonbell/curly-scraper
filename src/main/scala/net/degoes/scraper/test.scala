package net.degoes.scraper

import java.nio.file.{Path, Paths}

import scalaz.Monoid
import scalaz.zio.{App, ExitResult, IO, Promise}
import scalaz._
import Scalaz.{mzero, _}
import net.degoes.scraper.test.{Home, Processor, getURL}
import net.degoes.scraper.url.URL
import scalaz.zio.console._

object test extends App {
  val Home = URL("http://scalaz.org").get
  val Index = URL("http://scalaz.org/index.html").get
  val ScaladocIndex = URL("http://scalaz.org/scaladoc/index.html").get
  val About = URL("http://scalaz.org/about").get

  val SiteIndex =
    Map(
      Home -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
      Index -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
      ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
      About -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
    )

  val getURL: URL => IO[Exception, String] =
    (url: URL) =>
      SiteIndex
        .get(url)
        .fold[IO[Exception, String]](
          IO.fail(new Exception("Could not connect to: " + url)))(IO.now(_))


  val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) => IO.now(List(url -> html))

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Starting")
      rs <- scraper.crawlIOPar(
        Set(Home),
        models.stayInSeedDomainRouter(Set(Home)),
        Processor,
        getURL
        )
      print = rs.value.map(_._1).mkString("\n")
      _ <- putStrLn(s"results : \n$print")
    } yield
      ()).redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )
}


object test1 extends App {

  val rootFilePath = Paths.get("/Users/abell/temp1")
  val start = Set(
    URL("https://scalaz.github.io/7/").get
  )
  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Starting")
      rs <- scraper.crawlIOPar(
        start,
        models.stayInSeedDomainRouter(start),
        models.returnAndCache(rootFilePath),
        models.getURLCached(rootFilePath)
      )
      print = rs.value.map(_._1).mkString("\n")
      _ <- putStrLn(s"results : \n$print")
    } yield
      ()).redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )
}
