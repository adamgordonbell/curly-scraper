package com.cascadeofinsights.util

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import com.cascadeofinsights.scraper.models.URL
import scalaz.zio.IO

import scala.util.Try

object Cache {

  def getFromDisk(rootPath: Path)(digest: SHA256Hash): IO[Nothing, Option[String]] = {
    IO.sync {
      val path = Paths.get(rootPath.toAbsolutePath.toString + "/" + digest)
      if (Files.exists(path)) {
        Some(new String(Files.readAllBytes(path), UTF_8))
      } else {
        None
      }
    }
  }

  def writeToDisk(rootPath : Path): (SHA256Hash, String) => IO[Unit, Unit] =
    (digest, html) => {
      IO.sync {
        val writer = Try(new FileWriter(new File(rootPath.toAbsolutePath.toString + "/" + digest)))
        writer.map(w => {w.write(html); w}).recoverWith{case _ => writer}.map(_.close)
      }
    }
}
