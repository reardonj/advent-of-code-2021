package com.jmreardon.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day2Spec extends AnyFlatSpec with Matchers:
  val stream = Seq(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )

  "part a" should "yield 150" in {
    Day2.a(stream.iterator) mustBe 150
  }

  "part a" should "yield 900" in {
    Day2.b(stream.iterator) mustBe 900
  }
