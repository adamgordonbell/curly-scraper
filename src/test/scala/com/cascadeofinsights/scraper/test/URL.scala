package com.cascadeofinsights.scraper.test

import com.cascadeofinsights.scraper.models
import utest._

object URL extends TestSuite{
  val url = models.URL("https://scalaz.github.io/7/").get
  val tests = Tests {
    "relative url" - {
        url.relative("/7/index.html") ==> models.URL("https://scalaz.github.io/7/index.html")
        url.relative("/7/css/style.css") ==> models.URL("https://scalaz.github.io/7/css/style.css")
    }
  }
}
