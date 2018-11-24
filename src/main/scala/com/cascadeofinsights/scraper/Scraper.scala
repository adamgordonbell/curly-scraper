package com.cascadeofinsights.scraper

import scalaz.Monoid
import scalaz.zio._
import scalaz._
import Scalaz.{mzero, _}
import com.cascadeofinsights.scraper.models.{Crawl, Gets, URL}

object Scraper {

  def crawlIOPar[E: Monoid, A: Monoid](
    seeds: Set[URL],
    router: URL => Set[URL],
    processor: (URL, String) => IO[E, A],
    getURL: URL => IO[Exception, String] = Gets.getURL(_)
  ): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[(Crawl[E, A], Set[URL])]): IO[Nothing, Unit] = {
      println(s"\tLoop with seeds : $seeds")
      IO.parTraverse(seeds)(url =>
        getURL(url).redeem(
          _ => IO.unit,
          html =>
            processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _)).flatMap { crawl1 =>
              val urls = URL.extractURLs(url, html).toSet.flatMap(router)

              ref.modify {
                case (crawl0, visited) =>
                  (visited, (crawl0 |+| crawl1, visited ++ urls))
              }.flatMap(visited =>
                loop(urls -- visited, ref)
              )
            }
        )).void: IO[Nothing, Unit]
    }
    for {
      ref <- Ref(mzero[Crawl[E, A]] -> seeds)
      _ <- loop(seeds, ref)
      tuple <- ref.get
    } yield tuple._1
  }
}
