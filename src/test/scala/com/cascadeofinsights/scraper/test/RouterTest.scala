package com.cascadeofinsights.scraper.test

import com.cascadeofinsights.scraper.models
import com.cascadeofinsights.scraper.models.{Routers, URL}
import utest._

object RouterTest extends TestSuite{
  val url = URL("https://scalebythebay2018.sched.com/").get
  val tests = Tests {
    "stay in subdomain url" - {
      val router = Routers.stayInSeedDomainRouter(Set(url))
      router(URL("https://sched.com/").get) ==> Set.empty
    }
  }
}
