package com.coreyoconnor.stoa

trait Types[Rep] {
  def bool: Rep
  def int: Rep
  def function: Rep => Rep => Rep
}
