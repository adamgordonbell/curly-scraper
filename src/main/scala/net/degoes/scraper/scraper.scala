package net.degoes.scraper

import scalaz.Monoid
import scalaz.zio._
import scalaz._
import Scalaz.{mzero, _}

object scraper {

 def crawlIOPar[E: Monoid, A: Monoid](
  seeds     : Set[URL],
  router    : URL => Set[URL],
  processor : (URL, String) => IO[E, A],
  getURL    : URL => IO[Exception, String] = getURL(_)
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
 object Crawl {
  implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
   new Monoid[Crawl[E, A]]{
    def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
    def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
     Crawl(l.error |+| r.error, l.value |+| r.value)
   }
 }

 final case class URL private (parsed: io.lemonlabs.uri.Url) {
  import io.lemonlabs.uri._

  final def relative(page: String): Option[URL] =
   scala.util.Try(parsed.path match {
    case Path(parts) =>
     val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

     parsed.withPath(UrlPath(whole))
   }).toOption.map(new URL(_))

  def url: String = parsed.toString

  override def equals(a: Any): Boolean = a match {
   case that : URL => this.url == that.url
   case _ => false
  }

  override def hashCode: Int = url.hashCode
 }

 object URL {
  import io.lemonlabs.uri._

  def apply(url: String): Option[URL] =
   scala.util.Try(AbsoluteUrl.parse(url)).toOption match {
    case None => None
    case Some(parsed) => Some(new URL(parsed))
   }
 }

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

 def extractURLs(root: URL, html: String): List[URL] = {
  val pattern = "href=[\"\']([^\"\']+)[\"\']".r

  scala.util.Try({
   val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

   for {
    m   <- matches
    url <- URL(m).toList ++ root.relative(m).toList
   } yield url
  }).getOrElse(Nil)
 }
}
