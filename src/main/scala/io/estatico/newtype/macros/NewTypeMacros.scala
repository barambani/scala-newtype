package io.estatico.newtype.macros

import scala.reflect.macros.blackbox

private[macros] object NewTypeMacros {

  def newtypeAnnotation(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {

    import c.universe._

    def fail(msg: String) = c.abort(c.enclosingPosition, msg)

    def run() = annottees match {
      case List(clsDef: ClassDef) => runClass(clsDef)
      case List(clsDef: ClassDef, modDef: ModuleDef) => runClassWithObj(clsDef, modDef)
      case _ => fail("Unsupported newtype definition")
    }

    def runClass(clsDef: ClassDef) = {

      val ClassDef(mods, typeName, tparams, template) = clsDef
      val Template(parents, _, body) = template

      val ctor = getConstructor(body)
      val valDef = extractConstructorValDef(ctor)
      val shouldGenerateApplyMethod = mods.hasFlag(Flag.CASE)

      val shouldGenerateValExtensionMethod = body.collectFirst {
        case vd: ValDef if vd.mods.hasFlag(Flag.PARAMACCESSOR) && vd.name == valDef.name => ()
      }.isDefined

      if (hasUnsupportedExtends(parents)) fail(s"newtypes do not support inheritance")

      val baseRefinementName = TypeName(typeName.decodedName + "$newtype")

      if (tparams.isEmpty) {

        val maybeApplyMethod = if (!shouldGenerateApplyMethod) Nil else List(
          q"def apply(${valDef.name}: ${valDef.tpt}): Type = ${valDef.name}.asInstanceOf[Type]"
        )

        val maybeValDefMethod = if (!shouldGenerateValExtensionMethod) Nil else List(
          q"def ${valDef.name}: ${valDef.tpt} = repr.asInstanceOf[${valDef.tpt}]"
        )

        val extensionMethods = maybeValDefMethod

        val maybeOpsDef = if (maybeValDefMethod.isEmpty) Nil else List(
          q"""
            implicit final class Ops$$newtype(private val repr: Type) extends AnyVal {
              ..$extensionMethods
            }
          """
        )

        q"""
          type $typeName = ${typeName.toTermName}.Type
          object ${typeName.toTermName} {
            type Base = { type $baseRefinementName }
            trait Tag
            type Type = Base with Tag
            ..$maybeApplyMethod
            ..$maybeOpsDef
          }
        """
      } else {
        ???
      }
    }

    def getConstructor(body: List[Tree]): DefDef = body.collectFirst {
      case dd: DefDef if dd.name == termNames.CONSTRUCTOR => dd
    }.getOrElse(fail("Failed to locate constructor"))

    def extractConstructorValDef(dd: DefDef): ValDef = dd.vparamss match {
      case List(List(vd)) => vd
      case _ => fail("Unsupported constructor, must have exactly one argument")
    }

    def runClassWithObj(clsDef: ClassDef, modDef: ModuleDef) = {
      val q"object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        = modDef
      ???
    }

    def hasUnsupportedExtends(parents: List[Tree]): Boolean = {
      val ignoredExtends = List(tq"scala.Product", tq"scala.Serializable")
      !parents.forall(t => ignoredExtends.exists(t.equalsStructure))
    }

    run()
  }
}
