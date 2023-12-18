package test

import org.mojoz.querease.QuereaseMacros
import org.tresql.parsing

class QuereaseTestMacros extends QuereaseMacros {
  def no_args_macro(implicit p: parsing.QueryParsers) = {
    /** Do not use tresql macro_ interpolator due to java/scala version compatibility issues */
    p.parseExp("'no_args'")
  }
}
