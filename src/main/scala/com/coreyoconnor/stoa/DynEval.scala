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

  implicit object EvalExpr extends Expr[Eval] {
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
}
