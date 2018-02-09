package io.estatico.newtype.macros

import org.scalatest.{FlatSpec, Matchers}

class NewTypeMacrosTest extends FlatSpec with Matchers {

  behavior of "@newtype"

  it should "work" in {
    @newtype case class Foo(value: Int)
    val res: Foo = Foo(1)
    res shouldBe 1
    res shouldBe Foo(1)

    // The newtype should be implemented in such a way as to prevent type pattern matching.
    // res match {
    //   case _: Foo => ???
    //   case _ => ???
    // }
  }
}
