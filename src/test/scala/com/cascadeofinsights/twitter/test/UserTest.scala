package com.cascadeofinsights.twitter.test

import com.cascadeofinsights.scraper.models.TwitterName
import com.cascadeofinsights.twitter.UserLookup
import com.cascadeofinsights.scraper.devUtils.replRTS._
import utest._

object UserTest extends TestSuite{
  val user =  TwitterName("adamgordonbell").toList
  val tests = Tests {
      UserLookup.lookupProfile(user).unsafeRun ==> Seq(("Adam Gordon Bell", 304))
    }
  }

