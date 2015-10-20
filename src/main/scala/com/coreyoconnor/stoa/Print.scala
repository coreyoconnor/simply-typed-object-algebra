package com.coreyoconnor.stoa

object Print {
  implicit object PrintTypes extends Types[String] {
    def bool = "Bool"
    def int = "Int"
    def function = a => b => "(" + a + " => " + b + ")"
  }

  implicit object PrintExpr extends Expr[String] {
    type TypeRep = String
    val types = implicitly[Types[TypeRep]]
    def ref = index => "(var " + index.toString + ")"
    def abs = typeRep => rep => "(_: " + typeRep + " => " + rep + ")"
    def app = function => params => function + " " + params
    def int = int => int.toString
    def bool = bool => bool.toString
  }
}