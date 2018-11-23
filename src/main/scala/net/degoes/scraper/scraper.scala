package net.degoes.scraper

import scalaz.Monoid
import scalaz.zio._
import scalaz._
import Scalaz.{mzero, _}
import net.degoes.scraper.url.URL
import net.degoes.scraper.url._

object scraper {

 def crawlIOPar[E: Monoid, A: Monoid](
  seeds     : Set[URL],
  router    : URL => Set[URL],
  processor : (URL, String) => IO[E, A],
  getURL    : URL => IO[Exception, String] = models.getURL(_)
 ): IO[Nothing, Crawl[E, A]] = {
  def loop(seeds: Set[URL], ref: Ref[(Crawl[E, A], Set[URL])]): IO[Nothing, Unit] =
   IO.parTraverse(seeds)(url =>
    getURL(url).redeem(
     _    => IO.unit,
     html =>
      processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _)).flatMap { crawl1 =>
       val urls = extractURLs(url, html).toSet.flatMap(router)

       ref.modify {
        case (crawl0, visited) =>
         (visited, (crawl0 |+| crawl1, visited ++ urls))
       }.flatMap(visited =>
        loop(urls -- visited, ref)
       )
      }
    )).void : IO[Nothing, Unit]

  for {
   ref   <- Ref(mzero[Crawl[E, A]] -> seeds)
   _     <- loop(seeds, ref)
   tuple <- ref.get
  } yield tuple._1
 }

 final case class Crawl[E, A](error: E, value: A) {
  def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)
  def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
 }

 //maybe this can be derived somehow, since it is the monoid for all products?
 object Crawl {
  implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
   new Monoid[Crawl[E, A]]{
    def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
    def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
     Crawl(l.error |+| r.error, l.value |+| r.value)
   }
 }
}
