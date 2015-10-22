package com.coreyoconnor.stoa

object IntPrintExprs {
  import Print._
  class IntPrintExprs extends PrintExprs with IntExprs[String] {
    def LT = left => right => left + " < " + right
    def EQ = left => right => left + " == " + right
    def IF = cond => ifTrue => ifFalse =>
      "if (" + cond + ") { " + ifTrue + " } else { " + ifFalse + " }"
  }
  implicit val IntPrintExprs: IntExprs[String] = new IntPrintExprs
}
