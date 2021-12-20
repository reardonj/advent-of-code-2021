package com.jmreardon.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day1Spec extends AnyFlatSpec with Matchers:
  val depthStream =
    Seq(
      "199",
      "200",
      "208",
      "210",
      "200",
      "207",
      "240",
      "269",
      "260",
      "263"
    )

  "part a" should "increase depth 7 times" in {
    Day1.a(depthStream.iterator) mustBe 7
  }

  "part b" should "increase depth 5 times" in {
    Day1.b(depthStream.iterator) mustBe 5
  }
