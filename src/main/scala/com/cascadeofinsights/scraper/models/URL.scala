package com.cascadeofinsights.scraper.models

import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils

import scala.util.Try


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

  val digest : SHA256Hash = SHA256Hash.create(parsed.toString())

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
