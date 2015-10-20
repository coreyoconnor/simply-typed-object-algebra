package com.coreyoconnor.stoa

import scalaz._, Scalaz._

object Main {
  import Print._

  def testTypes[Rep](implicit types: Types[Rep]): Seq[Rep] = {
    import types._

    Seq(
      bool,
      int,
      function(bool)(bool)
    )
  }

  def printTestTypes(): Unit = {
    testTypes.foreach { t: String =>
      println(t)
    }
  }

  def testExprs[Rep](implicit expr: Expr[Rep]): Seq[Rep] = {
    import expr._

    val idBool = app(abs(types.bool)(ref(0)))

    Seq(
      ref(0),
      abs(types.bool)(ref(0)),
      idBool(bool(true)),
      abs(types.function(types.bool)(types.bool))(app(ref(0))(bool(false)))
    )
  }

  def testExprs1[Rep](implicit expr: Expr[Rep]): Seq[Rep] = {
    import expr._

    val idBool = app(abs(types.bool)(ref(0)))

    Seq(
      bool(false),
      int(42),
      idBool(bool(true)),
      app {
        abs(types.function(types.bool)(types.bool)) {
          app(ref(0))(bool(false))
        }
      } {
        abs(types.bool)(ref(0))
      }
    )
  }

  def printTestExprs(): Unit = {
    testExprs.foreach { t: String =>
      println(t)
    }
  }

  def evalTestExprs(): Unit = {
    testExprs1[DynEval.Eval].foreach { eval =>
      val value = eval(Monoid[DynEval.Heap].zero)
      println(value)
    }
  }

  def main(args: Array[String]) = {
    printTestTypes()
    printTestExprs()
    evalTestExprs()
  }
}
