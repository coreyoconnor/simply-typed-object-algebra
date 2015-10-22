package com.coreyoconnor.stoa

import scalaz._, Scalaz._

import scala.util.Try

object Main {
  import IntPrintExprs._
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

  def testExprs[Rep](implicit expr: Exprs[Rep]): Seq[Rep] = {
    import expr._

    val idBool = app(abs(types.bool)(ref(0)))

    Seq(
      bool(false),
      int(42),
      ref(0),
      abs(types.bool)(ref(0)),
      idBool(bool(true)),
      abs(types.function(types.bool)(types.bool))(app(ref(0))(bool(false))),
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
    testExprs zip Stream.from(1) foreach { case (t: String, index) =>
      println(index + ". " + t)
    }
  }

  def evalTestExprs(): Unit = {
    testExprs[DynEval.Eval] zip Stream.from(1) foreach { case (eval, index) =>
      Try {
        val value = eval(Monoid[DynEval.Heap].zero)
        println(index + ". " + value)
      }.recover {
        case ex: Throwable => println(index + ". ERROR: " + ex.toString)
      }
    }
  }

  def testIntExprs[Rep](implicit exprs: IntExprs[Rep]): Seq[Rep] = {
    import exprs._

    val idBool = app(abs(types.bool)(ref(0)))

    testExprs(exprs) ++ Seq(
      IF(bool(false)) {
        ref(0)
      } {
        int(42)
      },
      IF(LT(int(42))(int(256))) {
        idBool(bool(true))
      } {
        abs(types.function(types.int)(types.bool))(bool(true))
      },
      IF(EQ(LT(int(42))(int(256)))(bool(true))) {
        idBool(bool(true))
      } {
        abs(types.function(types.int)(types.bool))(bool(true))
      }
    )
  }

  def printTestIntExprs(): Unit = {
    testIntExprs zip Stream.from(1) foreach { case (t: String, index) =>
      println(index + ". " + t)
    }
  }

  def main(args: Array[String]) = {
    printTestTypes()
    printTestExprs()
    evalTestExprs()
    printTestIntExprs()
  }
}
