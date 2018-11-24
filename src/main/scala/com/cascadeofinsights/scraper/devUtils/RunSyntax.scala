package com.cascadeofinsights.scraper.devUtils

import scalaz.zio.{IO, RTS}

object replRTS extends RTS {
  implicit class RunSyntax[E, A](io: IO[E, A]){
    def unsafeRun: A = replRTS.unsafeRun(io)
  }
}
