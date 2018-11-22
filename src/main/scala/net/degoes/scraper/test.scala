package net.degoes.scraper

import scalaz.Monoid
import scalaz.zio.{App, ExitResult, IO, Promise}
import scalaz._
import Scalaz.{mzero, _}
import scalaz.zio.console._
import net.degoes.scraper.scraper.URL

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

  val ScalazRouter: URL => Set[URL] =
    url => if (url.parsed.apexDomain == Some("scalaz.org")) Set(url) else Set()

  val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) => IO.now(List(url -> html))

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Starting")
      rs <- scraper.crawlIOPar(
        Set(Home,ScaladocIndex,Index, About),
        ScalazRouter,
        Processor,
        getURL
        )
      print = rs.value.map(_._1).mkString("\n")
      _ <- putStrLn(s"results : $print")
    } yield
      ()).redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )
}
