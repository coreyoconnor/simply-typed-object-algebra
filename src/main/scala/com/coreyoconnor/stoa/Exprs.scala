package com.coreyoconnor.stoa

trait Exprs[Rep] {
  type TypeRep
  val types: Types[TypeRep]
  def ref: Int => Rep
  def abs: TypeRep => Rep => Rep
  def app: Rep => Rep => Rep
  def int: Int => Rep
  def bool: Boolean  => Rep
}
