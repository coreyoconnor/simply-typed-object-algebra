package com.coreyoconnor.stoa

trait IntExprs[Obj] extends Exprs[Obj] {
  def LT: Obj => Obj => Obj
  def EQ: Obj => Obj => Obj
  def IF: Obj => Obj => Obj => Obj
}
