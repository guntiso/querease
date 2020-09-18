package org.mojoz.querease

import org.tresql.macro_._
import org.tresql.parsing.{ Exp, QueryParsers }

class QuereaseMacros extends org.tresql.Macros {
  def if_not(implicit p: QueryParsers, bool: Exp, exprThen: Exp) =
    macro_"""case($bool, null, $exprThen)"""
}

object QuereaseMacros extends QuereaseMacros
