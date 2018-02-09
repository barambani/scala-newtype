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
  }

  // The newtype should be implemented in such a way as to prevent type pattern matching.
  // Unfortunately, assertDoesNotCompile and assertTypeError fail at compile time with this,
  // so we're not able to make a test case for it yet.
  // res match {
  //   case _: Foo => ???
  //   case _ => ???
  // }

  it should "generate an accessor extension method" in {
    Foo(1).value shouldBe 1
  }

  it should "convert instance methods into extension methods" in {
    val res: Bar = Bar(2).twice
    res shouldBe 4
  }

  behavior of "@newtype class"

  it should "not expose a default constructor" in {
    assertTypeError("""Baz("foo")""")
    Baz.create("foo") shouldBe "FOO"
  }

  it should "not expose its constructor argument by default" in {
    assertDoesNotCompile("""Baz.create("foo").value""")
  }

  it should "expose its constructor argument if defined as a val" in {
    Baz2.create("foo").value shouldBe "FOO"
  }

  behavior of "@newtype with type arguments"

  it should "generate a proper constructor" in {
    val repr = List(Option(1))
    val ot = OptionT(repr)
    // Assert that type is correct and that it has the same representation as repr.
    (ot: OptionT[List, Int]) shouldBe repr
  }

  it should "be Coercible" in {
    val repr = List(Option(1))
    val ot = OptionT(repr)

    val x = ot.coerce[List[Option[Int]]]
    x shouldBe repr

    val y = Vector(repr).coerce[Vector[OptionT[List, Int]]]
    y shouldBe Vector(repr)
  }

  it should "not coerce array types" in {
    val repr = List(Option(1))
    val ot = OptionT(repr)
    assertTypeError("Array(ot).coerce[Array[List[Option[Int]]]]")
  }
}

object NewTypeMacrosTest {

  @newtype case class Foo(value: Int)

  @newtype case class Bar(value: Int) {
    def twice: Bar = Bar(value * 2)
  }

  @newtype class Baz(value: String)
  object Baz {
    def create(value: String): Baz = value.toUpperCase.coerce[Baz]
  }

  @newtype class Baz2(val value: String)
  object Baz2 {
    def create(value: String): Baz2 = value.toUpperCase.coerce[Baz2]
  }

  @newtype case class OptionT[F[_], A](value: F[Option[A]])
}
