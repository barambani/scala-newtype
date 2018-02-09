package io.estatico.newtype.macros

import scala.reflect.macros.blackbox

private[macros] object NewTypeMacros {

  def newtypeAnnotation(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {

    import c.universe._

    q"""
      type Foo = Foo.Type
      object Foo {
        type Base = { type newtype$$Foo }
        trait Tag
        type Type = Base with Tag
        def apply(value: Int): Type = value.asInstanceOf[Type]
      }
    """
  }
}
