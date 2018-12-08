package com.cascadeofinsights.scraper.models

import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils

import scala.util.Try

object SHA256Hash {
  def parse(potentiallyValidHash: String): Option[SHA256Hash] = Try(new SHA256Hash(potentiallyValidHash)).toOption
  def create(input : String) : SHA256Hash = new SHA256Hash(DigestUtils.sha256(input))
  def unapply(raw: String): Option[SHA256Hash] = parse(raw)
}

class SHA256Hash(potentiallyValidHash: String) {
  def this(hashAsBytes: Array[Byte]) = {
    this(Hex.encodeHexString(hashAsBytes))
  }

  private val potentiallyValidHashNoPrefix = potentiallyValidHash.stripPrefix("sha256:")
  lazy val asStringWithPrefix: String = "sha256:" + potentiallyValidHashNoPrefix

  override def equals(other: Any): Boolean = {
    other match {
      case otherHash: SHA256Hash => otherHash.asStringWithPrefix == this.asStringWithPrefix
      case _ => false
    }
  }

  override def hashCode(): Int = {
    this.asStringWithPrefix.hashCode //sloppy but it works
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[SHA256Hash]

  override def toString: String = asStringWithPrefix
}
