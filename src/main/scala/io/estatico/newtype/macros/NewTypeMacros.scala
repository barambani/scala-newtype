package io.estatico.newtype.macros

import io.estatico.newtype.Coercible
import scala.reflect.macros.blackbox

private[macros] object NewTypeMacros {

  def newtypeAnnotation(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {

    import c.universe._

    val CoercibleCls = typeOf[Coercible[Nothing, Nothing]].typeSymbol
    val CoercibleObj = CoercibleCls.companion

    // We need to know if the newtype is defined in an object so we can report
    // an error message if methods are defined on it (otherwise, the user will
    // get a cryptic error of 'value class may not be a member of another class'
    // due to our generated extension methods.
    val isDefinedInObject = c.internal.enclosingOwner.isModuleClass

    def fail(msg: String) = c.abort(c.enclosingPosition, msg)

    // Entry point for the macro annotation.
    def run() = annottees match {
      case List(clsDef: ClassDef) => runClass(clsDef)
      case List(clsDef: ClassDef, modDef: ModuleDef) => runClassWithObj(clsDef, modDef)
      case _ => fail("Unsupported newtype definition")
    }

    def runClass(clsDef: ClassDef) = {
      runClassWithObj(clsDef, q"object ${clsDef.name.toTermName}".asInstanceOf[ModuleDef])
    }

    def runClassWithObj(clsDef: ClassDef, modDef: ModuleDef) = {
      val valDef = extractConstructorValDef(getConstructor(clsDef.impl.body))
      // Converts [F[_], A] to [F, A]; needed for applying the defined type params.
      val tparamNames: List[TypeName] = clsDef.tparams.map(_.name)
      // Type params with variance removed for building methods.
      val tparamsNoVar: List[TypeDef] = clsDef.tparams.map(td =>
        TypeDef(Modifiers(Flag.PARAM), td.name, td.tparams, td.rhs)
      )
      // Ensure we're not trying to inherit from anything.
      validateParents(clsDef.impl.parents)
      // Build the type and object definitions.
      generateNewType(clsDef, modDef, valDef, tparamsNoVar, tparamNames)
    }

    def generateNewType(
      clsDef: ClassDef, modDef: ModuleDef, valDef: ValDef,
      tparamsNoVar: List[TypeDef], tparamNames: List[TypeName]
    ): Tree = {
      val q"object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }" = modDef
      val typeName = clsDef.name
      val tparams = clsDef.tparams
      val baseRefinementName = TypeName(clsDef.name.decodedName + "$newtype")
      val companionExtraDefs =
        maybeGenerateApplyMethod(clsDef, valDef, tparamsNoVar, tparamNames) ++
        maybeGenerateOpsDef(clsDef, valDef, tparamsNoVar, tparamNames) ++
        generateCoercibleInstances(tparamsNoVar, tparamNames)

      if (tparams.isEmpty) {
        q"""
          type $typeName = $objName.Type
          object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
            ..$objDefs
            type Repr = ${valDef.tpt}
            type Base = { type $baseRefinementName }
            trait Tag
            type Type = Base with Tag
            ..$companionExtraDefs
          }
        """
      } else {
        q"""
          type $typeName[..$tparams] = ${typeName.toTermName}.Type[..$tparamNames]
          object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
            ..$objDefs
            type Repr[..$tparams] = ${valDef.tpt}
            type Base = { type $baseRefinementName }
            trait Tag[..$tparams]
            type Type[..$tparams] = Base with Tag[..$tparamNames]
            ..$companionExtraDefs
          }
        """
      }
    }

    def maybeGenerateApplyMethod(
      clsDef: ClassDef, valDef: ValDef, tparamsNoVar: List[TypeDef], tparamNames: List[TypeName]
    ): Option[Tree] = {
      if (!clsDef.mods.hasFlag(Flag.CASE)) None else Some(
        if (tparamsNoVar.isEmpty) {
          q"def apply(${valDef.name}: ${valDef.tpt}): Type = ${valDef.name}.asInstanceOf[Type]"
        } else {
          q"""
            def apply[..$tparamsNoVar](${valDef.name}: ${valDef.tpt}): Type[..$tparamNames] =
              ${valDef.name}.asInstanceOf[Type[..$tparamNames]]
          """
        }
      )
    }

    // We should expose the constructor argument as an extension method only if
    // it was defined as a public param.
    def shouldGenerateValMethod(clsDef: ClassDef, valDef: ValDef): Boolean =
      clsDef.impl.body.collectFirst {
        case vd: ValDef
          if vd.mods.hasFlag(Flag.PARAMACCESSOR)
            && !vd.mods.hasFlag(Flag.PRIVATE)
            && vd.name == valDef.name => ()
      }.isDefined

    def maybeGenerateValMethod(
      clsDef: ClassDef, valDef: ValDef
    ): Option[Tree] = {
      if (!shouldGenerateValMethod(clsDef, valDef)) {
        None
      } else if (!isDefinedInObject) {
        c.abort(valDef.pos, s"""
          |Fields can only be defined for newtypes defined in an object
          |Consider defining as: private val ${valDef.name.decodedName}
        """.trim.stripMargin)
      } else {
        Some(q"def ${valDef.name}: ${valDef.tpt} = repr.asInstanceOf[${valDef.tpt}]")
      }
    }

    def maybeGenerateOpsDef(
      clsDef: ClassDef, valDef: ValDef, tparamsNoVar: List[TypeDef], tparamNames: List[TypeName]
    ): Option[Tree] = {
      val extensionMethods =
        maybeGenerateValMethod(clsDef, valDef) ++ getInstanceMethods(clsDef)

      if (extensionMethods.isEmpty) {
        None
      } else {
        Some(
          if (clsDef.tparams.isEmpty) {
            q"""
              implicit final class Ops$$newtype(private val repr: Type) extends AnyVal {
                ..$extensionMethods
              }
            """
          } else {
            q"""
              implicit final class Ops$$newtype[..${clsDef.tparams}](
                private val repr: Type[..$tparamNames]
              ) extends AnyVal {
                ..$extensionMethods
              }
            """
          }
        )
      }
    }

    def generateCoercibleInstances(
      tparamsNoVar: List[TypeDef], tparamNames: List[TypeName]
    ): List[Tree] = {
      if (tparamsNoVar.isEmpty) List(
        q"@inline implicit def unsafeWrap: $CoercibleCls[Repr, Type] = $CoercibleObj.instance",
        q"@inline implicit def unsafeUnwrap: $CoercibleCls[Type, Repr] = $CoercibleObj.instance",
        q"@inline implicit def unsafeWrapM[M[_]]: $CoercibleCls[M[Repr], M[Type]] = $CoercibleObj.instance",
        q"@inline implicit def unsafeUnwrapM[M[_]]: $CoercibleCls[M[Type], M[Repr]] = $CoercibleObj.instance",
        // Avoid ClassCastException with Array types by prohibiting Array coercing.
        q"@inline implicit def cannotWrapArrayAmbiguous1: $CoercibleCls[Array[Repr], Array[Type]] = $CoercibleObj.instance",
        q"@inline implicit def cannotWrapArrayAmbiguous2: $CoercibleCls[Array[Repr], Array[Type]] = $CoercibleObj.instance",
        q"@inline implicit def cannotUnwrapArrayAmbiguous1: $CoercibleCls[Array[Type], Array[Repr]] = $CoercibleObj.instance",
        q"@inline implicit def cannotUnwrapArrayAmbiguous2: $CoercibleCls[Array[Type], Array[Repr]] = $CoercibleObj.instance"
      ) else List(
        q"@inline implicit def unsafeWrap[..$tparamsNoVar]: $CoercibleCls[Repr[..$tparamNames], Type[..$tparamNames]] = $CoercibleObj.instance",
        q"@inline implicit def unsafeUnwrap[..$tparamsNoVar]: $CoercibleCls[Type[..$tparamNames], Repr[..$tparamNames]] = $CoercibleObj.instance",
        q"@inline implicit def unsafeWrapM[M[_], ..$tparamsNoVar]: $CoercibleCls[M[Repr[..$tparamNames]], M[Type[..$tparamNames]]] = $CoercibleObj.instance",
        q"@inline implicit def unsafeUnwrapM[M[_], ..$tparamsNoVar]: $CoercibleCls[M[Type[..$tparamNames]], M[Repr[..$tparamNames]]] = $CoercibleObj.instance",
        // Avoid ClassCastException with Array types by prohibiting Array coercing.
        q"@inline implicit def cannotWrapArrayAmbiguous1[..$tparamsNoVar]: $CoercibleCls[Array[Repr[..$tparamNames]], Array[Type[..$tparamNames]]] = $CoercibleObj.instance",
        q"@inline implicit def cannotWrapArrayAmbiguous2[..$tparamsNoVar]: $CoercibleCls[Array[Repr[..$tparamNames]], Array[Type[..$tparamNames]]] = $CoercibleObj.instance",
        q"@inline implicit def cannotUnwrapArrayAmbiguous1[..$tparamsNoVar]: $CoercibleCls[Array[Type[..$tparamNames]], Array[Repr[..$tparamNames]]] = $CoercibleObj.instance",
        q"@inline implicit def cannotUnwrapArrayAmbiguous2[..$tparamsNoVar]: $CoercibleCls[Array[Type[..$tparamNames]], Array[Repr[..$tparamNames]]] = $CoercibleObj.instance"
      )
    }

    def getConstructor(body: List[Tree]): DefDef = body.collectFirst {
      case dd: DefDef if dd.name == termNames.CONSTRUCTOR => dd
    }.getOrElse(fail("Failed to locate constructor"))

    def extractConstructorValDef(ctor: DefDef): ValDef = ctor.vparamss match {
      case List(List(vd)) => vd
      case _ => fail("Unsupported constructor, must have exactly one argument")
    }

    def getInstanceMethods(clsDef: ClassDef): List[DefDef] = {
      val res = clsDef.impl.body.flatMap {
        case vd: ValDef =>
          if (vd.mods.hasFlag(Flag.CASEACCESSOR) || vd.mods.hasFlag(Flag.PARAMACCESSOR)) Nil
          else c.abort(vd.pos, "val definitions not supported, use def instead")
        case dd: DefDef =>
          if (dd.name == termNames.CONSTRUCTOR) Nil else List(dd)
        case x =>
          c.abort(x.pos, s"illegal definition in newtype: $x")
      }
      if (res.nonEmpty && !isDefinedInObject) {
        c.abort(res.head.pos, "Methods can only be defined for newtypes defined in an object")
      }
      res
    }

    def validateParents(parents: List[Tree]): Unit = {
      val ignoredExtends = List(tq"scala.Product", tq"scala.Serializable", tq"scala.AnyRef")
      val unsupported = parents.filterNot(t => ignoredExtends.exists(t.equalsStructure))
      if (unsupported.nonEmpty) {
        fail(s"newtypes do not support inheritance; illegal supertypes: ${unsupported.mkString(", ")}")
      }
    }

    run()
  }
}
