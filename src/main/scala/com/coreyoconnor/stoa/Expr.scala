package com.coreyoconnor.stoa

trait Expr[Rep] {
  type TypeRep
  val types: Types[TypeRep]
  def ref: Int => Rep
  def abs: TypeRep => Rep => Rep
  def app: Rep => Rep => Rep
  def int: Int => Rep
  def bool: Boolean  => Rep
  def LT: Rep => Rep => Rep
  def EQ: Rep => Rep => Rep
  def IF: Rep => Rep => Rep => Rep
}
