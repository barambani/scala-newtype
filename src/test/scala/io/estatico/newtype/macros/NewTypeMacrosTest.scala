package io.estatico.newtype.macros

import org.scalatest.{FlatSpec, Matchers}
import io.estatico.newtype.ops._

class NewTypeMacrosTest extends FlatSpec with Matchers {

  import NewTypeMacrosTest._

  behavior of "@newtype case class"

  it should "generate a type alias, companion object, and constructor" in {

    // Ensure that we can access the type and the constructor.
    val res: Foo = Foo(1)

    // Should have the same runtime representation as Int.
    res shouldBe 1
    res shouldBe Foo(1)

    // The newtype should be implemented in such a way as to prevent type pattern matching.
    // res match {
    //   case _: Foo => ???
    //   case _ => ???
    // }
  }

  it should "generate an accessor extension method" in {
    Foo(1).value shouldBe 1
  }

  behavior of "@newtype class"

  it should "not expose a default constructor" in {
    Baz("foo") shouldBe "FOO"
  }
}

object NewTypeMacrosTest {

  @newtype case class Foo(value: Int)

  @newtype class Baz(value: String)
  object Baz {
    def apply(value: String): Baz = value.toUpperCase.coerce[Baz]
  }
}
