package com.coreyoconnor.stoa

object IntPrintExprs {
  import Print._
  class IntPrintExprs extends PrintExpr with IntExprs[String] {
  }
  implicit val IntPrintExprs: IntExprs[String] = new IntPrintExprs
}
