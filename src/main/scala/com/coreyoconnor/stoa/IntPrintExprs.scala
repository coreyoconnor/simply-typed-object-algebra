package com.coreyoconnor.stoa

object IntPrintExprs {
  import Print._
  class IntPrintExprs extends PrintExprs with IntExprs[String] {
  }
  implicit val IntPrintExprs: IntExprs[String] = new IntPrintExprs
}
