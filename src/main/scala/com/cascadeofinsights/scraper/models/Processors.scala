package com.cascadeofinsights.scraper.models

import java.io.{File, FileWriter}
import java.nio.file.Path

import scalaz.zio.IO

import scala.util.Try

object Processors {

  val IdProcessor: (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) => IO.now(List(url -> html))

  def writeToCacheProcessor(rootPath : Path): (URL, String) => IO[Unit, Unit] =
    (url, html) => {
      IO.sync {
        val writer = Try(new FileWriter(new File(rootPath.toAbsolutePath.toString + "/" + url.digest)))
        writer.map(w => {w.write(html); w}).recoverWith{case _ => writer}.map(_.close)
      }
    }

  def returnAndCache(rootPath : Path) : (URL, String) => IO[Unit, List[(URL, String)]] =
    (url, html) =>
      for {
        _ <- writeToCacheProcessor(rootPath)(url, html)
        r <- IdProcessor(url,html)
      } yield r

}


