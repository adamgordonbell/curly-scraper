package net.degoes.scraper

import scalaz.Scalaz.{mzero, _}
import scalaz.{Monoid, _}
import scalaz.zio._

object url {

 final case class URL private (parsed: io.lemonlabs.uri.Url) {

  import io.lemonlabs.uri._

  final def relative(page: String): Option[URL] =
   if (page.contains("://")){
     None
   } else {
    scala.util.Try(parsed.path match {
     case Path(parts) =>
      val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

      parsed.withPath(UrlPath(whole))
    }).toOption.map(new URL(_))
   }

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
