package com.cascadeofinsights.scraper.models

object Routers {

  def stayInSeedDomainRouter(seed : Set[URL]) : URL => Set[URL] =
    url => {
      val rootApexs = seed.map(_.parsed.apexDomain).flatten
      url.parsed.apexDomain.map(apex => if(rootApexs.contains(apex)) Set(url) else Set(): Set[URL]).getOrElse(Set())
    }
}
