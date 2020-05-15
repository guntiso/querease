package test

trait FunctionSignatures
  extends CustomDbFunctionSignatures
     with PostgresFunctionSignatures

trait CustomDbFunctionSignatures {
  def checked_resolve[T](resolvable: String, resolved: Seq[T], error_message: String): T
}

trait PostgresFunctionSignatures {
  def array(query: Any): Any
  def coalesce[T](pars: T*): T
  def concat_ws(sep: String, str: Any*): String
}
