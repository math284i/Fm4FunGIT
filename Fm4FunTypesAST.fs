// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module Fm4FunTypesAST

type Variable = String
type Array = Aexpr List
and Aexpr =
  | Num of float
  | Var of Variable
  | ArrEntry of (Array*Aexpr)
  | MultExpr of (Aexpr * Aexpr)
  | DivExpr of (Aexpr * Aexpr)
  | AddExpr of (Aexpr * Aexpr)
  | MinusExpr of (Aexpr * Aexpr)
  | PowExpr of (Aexpr * Aexpr)
  | UMinusExpr of (Aexpr)

type Bexpr =
  | B of bool
  | AndExpr of (Bexpr*Bexpr)
  | OrExpr of (Bexpr*Bexpr)
  | ScAndExpr of (Bexpr*Bexpr)
  | ScOrExpr of (Bexpr*Bexpr)
  | NotExpr of Bexpr
  | EqualExpr of (Aexpr*Aexpr)
  | NotEqualExpr of (Aexpr*Aexpr)
  | GreaterThanExpr of (Aexpr*Aexpr)
  | GreaterOrEqualExpr of (Aexpr*Aexpr)
  | LessThanExpr of (Aexpr*Aexpr)
  | LessOrEqualExpr of (Aexpr*Aexpr)
  
type Cexpr =
  | AssignExpr of (Variable*Aexpr)
  | AssignToArrExpr of (Array*Aexpr*Aexpr)
  | SkipExpr
  | AlsoExpr of (Cexpr*Cexpr)
  | IfExpr of GuardedExpr
  | DoExpr of GuardedExpr
and GuardedExpr =
  | ArrowExpr of (Bexpr*Cexpr)
  | AlsoExpr of (GuardedExpr*GuardedExpr)
