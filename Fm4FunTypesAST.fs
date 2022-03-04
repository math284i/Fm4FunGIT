// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module Fm4FunTypesAST

type expr =
  | Num of float
  | MultExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | AddExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
