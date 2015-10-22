package com.coreyoconnor.stoa

object DynEval {
  sealed trait Type
  final case class BoolType() extends Type
  final case class IntType() extends Type
  // no codomain type: not usable under dynamic typing
  final case class FunctionType(domain: Type) extends Type

  sealed trait Val {
    def headType: Type
  }
  final case class IntVal(value: Int) extends Val {
    def headType: Type = IntType()
  }
  final case class BoolVal(value: Boolean) extends Val {
    def headType: Type = BoolType()
  }
  final case class AbsVal(variableType: Type, next: Eval) extends Val {
    def headType: Type = FunctionType(variableType)
  }

  type Heap = List[Val]
  type Eval = Heap => Val

  class EvalExprs extends Exprs[Eval] {
    type TypeRep = Type
    val types = new Types[Type] {
      def bool = BoolType()
      def int = IntType()
      def function = domain => _ => FunctionType(domain)
    }
    def ref = index                 => heap  => heap(index)
    def abs = typeReq  => expr      => heap  => AbsVal(typeReq, expr)
    def app = function => paramExpr => heap0 => {
      function(heap0) match {
        case AbsVal(typeReq, expr) => {
          val param = paramExpr(heap0)
          if (param.headType == typeReq) {
            val heap1 = param :: heap0
            expr(heap1)
          } else {
            sys.error(s"Value ${param} does not have type ${typeReq}")
          }
        }
        case _ => sys.error("Application is not to a function expresison")
      }
    }
    def int  = int  => heap => IntVal(int)
    def bool = bool => heap => BoolVal(bool)
  }

  implicit val EvalExprs: Exprs[Eval] = new EvalExprs

  class CondEvalExprs extends EvalExprs with CondExprs[Eval] {
    def LT = left => right => heap => {
      (left(heap), right(heap)) match {
        case (IntVal(leftInt), IntVal(rightInt)) => BoolVal(leftInt < rightInt)
        case _ => sys.error("type mismatch: LT must be applied to int values only.")
      }
    }
    def EQ = left => right => heap => {
      (left(heap), right(heap)) match {
        case (BoolVal(leftBool), BoolVal(rightBool)) => BoolVal(leftBool == rightBool)
        case (IntVal(leftInt)  , IntVal(rightInt)  ) => BoolVal(leftInt == rightInt)
          case _ => sys.error("type mismatch: EQ must be applied to two expressions of the same type.")
      }
    }
    def IF = cond => ifTrue => ifElse => heap => cond(heap) match {
      case BoolVal(true ) => ifTrue(heap)
      case BoolVal(false) => ifElse(heap)
    }
  }

  implicit val CondEvalExprs: CondExprs[Eval] = new CondEvalExprs
}
