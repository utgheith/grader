package ag.k2

import scala.collection.mutable
import scala.quoted.*

// Derives a Schema from a type, at compile time (see Schema).
object SchemaMacro {
  def derive[T: Type](using q: Quotes): Expr[Schema.Of[T]] = {
    val described = SchemaMacro(q).root[T]
    '{ new Schema.Of[T] { val schema: Schema = $described } }
  }
}

private final class SchemaMacro(val q: Quotes) {
  import q.reflect.*
  given Quotes = q

  // the records and enums found so far, by simple name, in order
  private val decls =
    mutable.LinkedHashMap[String, (Symbol, Expr[Schema.Decl])]()

  // Capture checking annotates types (e.g. with inferred capture sets);
  // they don't change the encoding.
  private def plain(t: TypeRepr): TypeRepr = t.dealias.widen match {
    case AnnotatedType(under, _) => plain(under)
    case other                   => other
  }

  private def fail(t: TypeRepr, why: String): Nothing =
    report.errorAndAbort(
      s"${t.show} can't be described for other languages: $why"
    )

  private def arg(t: TypeRepr, base: TypeRepr): TypeRepr =
    plain(t.baseType(base.typeSymbol).typeArgs.head)

  def tpe(t0: TypeRepr): Expr[Schema.Type] = {
    val t = plain(t0)
    val sym = t.typeSymbol
    if (t =:= TypeRepr.of[String]) '{ Schema.Type.Str }
    else if (t =:= TypeRepr.of[Boolean]) '{ Schema.Type.Bool }
    else if (t =:= TypeRepr.of[Int]) '{ Schema.Type.Int }
    else if (t =:= TypeRepr.of[Double]) '{ Schema.Type.Double }
    else if (t =:= TypeRepr.of[Long])
      fail(t, "upickle writes a big Long as a string; use Int, or a String")
    else if (t <:< TypeRepr.of[Option[Any]]) {
      val of = tpe(arg(t, TypeRepr.of[Option[Any]]))
      '{ Schema.Type.Optional($of) }
    } else if (t.baseClasses.contains(TypeRepr.of[Map[Any, Any]].typeSymbol)) {
      // (by class: maps are invariant in their keys)
      val List(k, v) =
        t.baseType(TypeRepr.of[Map[Any, Any]].typeSymbol)
          .typeArgs
          .map(plain): @unchecked
      if (!(k =:= TypeRepr.of[String]))
        fail(t, "only maps with String keys are objects")
      val of = tpe(v)
      '{ Schema.Type.Dict($of) }
    } else if (t <:< TypeRepr.of[Seq[Any]]) {
      val of = tpe(arg(t, TypeRepr.of[Seq[Any]]))
      '{ Schema.Type.List($of) }
    } else if (sym == TypeRepr.of[Sealed[Any]].typeSymbol) {
      val of = tpe(plain(t.typeArgs.head))
      '{ Schema.Type.Sealed($of) }
    } else if (sym.flags.is(Flags.Enum) && !sym.flags.is(Flags.Case)) {
      val name = Expr(declare(t, enumDecl(t)))
      '{ Schema.Type.Enum($name) }
    } else if (sym.isClassDef && sym.flags.is(Flags.Case)) {
      val name = Expr(declare(t, recordDecl(t)))
      '{ Schema.Type.Record($name) }
    } else fail(t, "not in the portable subset (see Schema)")
  }

  // `t`'s declaration, made once; returns its name
  private def declare(t: TypeRepr, make: => Expr[Schema.Decl]): String = {
    val sym = t.typeSymbol
    val name = sym.name
    decls.get(name) match {
      case Some((other, _)) if other != sym =>
        fail(t, s"another type named $name (${other.fullName}) is used too")
      case Some(_) => ()
      case None    =>
        // (a placeholder first, so that a recursive type ends)
        decls(name) = (sym, '{ ??? })
        decls(name) = (sym, make)
    }
    name
  }

  private def enumDecl(t: TypeRepr): Expr[Schema.Decl] = {
    val sym = t.typeSymbol
    val cases = sym.children
    if (cases.exists(_.isClassDef))
      fail(t, "an enum case with fields (only simple cases are portable)")
    val name = Expr(sym.name)
    val names = Expr(cases.map(_.name))
    '{ Schema.Decl.Enum($name, $names) }
  }

  private def recordDecl(t: TypeRepr): Expr[Schema.Decl] = {
    val sym = t.typeSymbol
    if (t.typeArgs.nonEmpty) fail(t, "a generic case class")
    val companion = Ref(sym.companionModule)
    val fields = sym.caseFields.zipWithIndex.map { (f, i) =>
      val ft = plain(t.memberType(f))
      val shape = tpe(ft)
      // the default's getter, if it has one
      val getter =
        sym.companionClass
          .declaredMethod(s"$$lessinit$$greater$$default$$${i + 1}")
          .headOption
      val default: Expr[Option[ujson.Value]] = getter match {
        case None    => '{ None }
        case Some(g) =>
          ft.asType match {
            case '[a] =>
              val writer = Expr
                .summon[upickle.default.Writer[a]]
                .getOrElse(
                  fail(ft, "no upickle Writer for it")
                )
              val value = companion.select(g).asExprOf[a]
              '{ Some(upickle.default.writeJs[a]($value)(using $writer)) }
          }
      }
      val name = Expr(f.name)
      '{ Schema.Field($name, $shape, $default) }
    }
    val name = Expr(sym.name)
    val all = Expr.ofSeq(fields)
    '{ Schema.Decl.Record($name, $all) }
  }

  def root[T: Type]: Expr[Schema] = {
    val top = plain(TypeRepr.of[T])
    if (!(top.typeSymbol.isClassDef && top.typeSymbol.flags.is(Flags.Case)))
      fail(top, "an event has to be a case class")
    val _ = tpe(top)
    val full = Expr(top.typeSymbol.fullName)
    val all = Expr.ofSeq(decls.values.map(_._2).toSeq)
    '{ Schema($full, $all) }
  }
}
