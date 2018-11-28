package com.cascadeofinsights.scraper.test

import com.cascadeofinsights.scraper.models
import com.cascadeofinsights.scraper.models.URL
import utest._

object URLTest extends TestSuite{
  val url = models.URL("https://scalaz.github.io/7/").get
  val tests = Tests {
    "relative url" - {
        url.relative("/7/index.html") ==> URL("https://scalaz.github.io/7/index.html")
        url.relative("/7/css/style.css") ==> URL("https://scalaz.github.io/7/css/style.css")
    }
  }
}
