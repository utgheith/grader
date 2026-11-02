package ag.k2

// ---- What an event looks like in the stream, for other languages
//
// A Schema describes how upickle encodes a type, as far as other languages
// need to know to read and write it. It's derived from the Scala types at
// compile time (Schema.Of), so Scala stays the source of truth; bindings
// for other languages are generated from it (see Bindings).
//
// Only a portable subset of types is allowed, and anything else is a
// compile error where its EventKind is made:
//   String, Boolean, Int, Double
//   Seq[A] (an array), Option[A] (the value, or null)
//   Map[String, A] (an object)
//   case classes (objects; not generic)
//   enums whose cases are all simple (the case's name, as a string)
//   Sealed[A] ({"key", "nonce", "data"})
// (Not Long: upickle writes big ones as strings.)
//
// upickle leaves out a field whose value is its default, so a field with a
// default may be missing; its default is kept here, as JSON.
final case class Schema(
    // the type described, by its full Scala name
    root: String,
    // the records and enums it uses, the root's first, each named by its
    // simple name (two with the same simple name are an error)
    decls: Seq[Schema.Decl]
)

object Schema {
  enum Type {
    case Str, Bool, Int, Double
    case List(of: Type)
    case Optional(of: Type)
    case Dict(of: Type)
    case Record(name: String)
    case Enum(name: String)
    case Sealed(of: Type)
  }

  final case class Field(name: String, tpe: Type, default: Option[ujson.Value])

  enum Decl {
    case Record(name: String, fields: Seq[Field])
    case Enum(name: String, cases: Seq[String])

    def name: String
  }

  // the schema of T
  trait Of[T] {
    def schema: Schema
  }

  object Of {
    inline given derived[T]: Of[T] = ${ SchemaMacro.derive[T] }
  }
}
