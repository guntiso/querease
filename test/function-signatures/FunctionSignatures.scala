package test

import org.tresql.compiling.TresqlFunctionSignatures

trait FunctionSignatures
  extends CustomDbFunctionSignatures
     with HsqldbFunctionSignatures
     with PostgresFunctionSignatures
     with TresqlFunctionSignatures

trait CustomDbFunctionSignatures {
  def checked_resolve[T](resolvable: String, resolved: Seq[T], error_message: String): T
}

trait HsqldbFunctionSignatures {
  def convert(value: Any, typeName: String): Any
}

trait PostgresFunctionSignatures {
  def array(query: Any): Any
  def concat_ws(sep: String, str: Any*): String
  def nullif(a: Integer, b: Integer): Integer
}
