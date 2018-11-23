package com.cascadeofinsights.scraper.models

import scalaz.Monoid
import scalaz.zio._
import scalaz._
import Scalaz.{mzero, _}
import scalaz.Monoid
import scalaz.Monoid._
import scalaz.Scalaz.mzero

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
