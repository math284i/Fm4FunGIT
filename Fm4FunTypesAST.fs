// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module Fm4FunTypesAST

open System

type Array = Aexpr List
and Aexpr =
  | Num of int
  | Var of string
  | ArrEntry of (string*Aexpr)
  | MultExpr of (Aexpr * Aexpr)
  | DivExpr of (Aexpr * Aexpr)
  | AddExpr of (Aexpr * Aexpr)
  | MinusExpr of (Aexpr * Aexpr)
  | PowExpr of (Aexpr * Aexpr)
  | UMinusExpr of (Aexpr)

type Bexpr =
  | True
  | False
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
  | AssignExpr of (string*Aexpr)
  | AssignToArrExpr of (string*Aexpr*Aexpr)
  | SkipExpr
  | DoubleExpr of (Cexpr*Cexpr)
  | IfExpr of GuardedExpr
  | DoExpr of GuardedExpr
and GuardedExpr =
  | ArrowExpr of (Bexpr*Cexpr)
  | AlsoExpr of (GuardedExpr*GuardedExpr)

type Pexpr =
  | True
  | PAndExpr of (Pexpr*Pexpr)
  | POrExpr of (Pexpr*Pexpr)
  | PNotExpr of Pexpr
  | PImplyExpr of (Pexpr*Pexpr)
  | PExistsExpr of (Eexpr*Pexpr)
  | PAllExpr of (Eexpr*Pexpr)
  | PEqualExpr of (Eexpr*Eexpr)
and Eexpr =
  | Logical
  | EAdd of (Eexpr*Eexpr)