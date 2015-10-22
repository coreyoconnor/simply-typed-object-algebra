package com.coreyoconnor.stoa

object CondPrintExprs {
  import Print._
  class CondPrintExprs extends PrintExprs with CondExprs[String] {
    def LT = left => right => left + " < " + right
    def EQ = left => right => left + " == " + right
    def IF = cond => ifTrue => ifFalse =>
      "if (" + cond + ") { " + ifTrue + " } else { " + ifFalse + " }"
  }
  implicit val CondPrintExprs: CondExprs[String] = new CondPrintExprs
}
