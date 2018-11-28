package com.cascadeofinsights.scraper.models

object Routers {

  type Route = URL => Set[URL]

  def stayInSeedDomainRouter(seed : Set[URL]) : Route=
    url => {
      val rootApexs = seed.map(_.parsed.apexDomain).flatten
      url.parsed.apexDomain.map(apex => if(rootApexs.contains(apex)) Set(url) else Set(): Set[URL]).getOrElse(Set())
    }

  val dropAnchorsAndQueryParams : Route =
    url => {
      val stripped = url.parsed.toString.takeWhile(char => char != '#' && char != '?')
      URL(stripped).toSet
    }

  def debug(name : String) : Route =
    url => {
      println(s"$name: $url")
      Set(url)
    }
  val id : Route = url => Set(url)

  //combining routes could be done with a klesi but sets break the monad laws so not supported
  //we could write a moniod instane for this instead
  def compose(routes : Route*): Route = {
   routes.foldLeft(id){ (acc : Route, r : Route) =>
     acc.andThen(g => g.flatMap(r))
   }
  }
}
