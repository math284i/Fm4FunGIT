// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "Fm4FunTypesAST.fs"
open Fm4FunTypesAST
#load "Fm4FunParser.fs"
open Fm4FunParser
#load "Fm4FunLexer.fs"
open Fm4FunLexer

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)

let variablesArray = []

let rec variableHelper str arr =
    match arr with
    | []                     -> failwith "Undefined variable"
    | (x,y)::xs when x = str -> y
    | x::xs                  -> variableHelper str xs

(*
let rec evalA e =
  match e with
    | Num(x) -> x
    | Var(x) -> variableHelper x variablesArray
    | ArrEntry (a, b) -> 0.0
    | MultExpr(x,y) -> evalA(x) * evalA (y)
    | DivExpr(x,y) -> evalA(x) / evalA (y)
    | AddExpr(x,y) -> evalA(x) + evalA (y)
    | MinusExpr(x,y) -> evalA(x) - evalA (y)
    | PowExpr(x,y) -> evalA(x) ** evalA (y)
    | UMinusExpr(x) -> - evalA(x)

*)

let rec evalA e =
  match e with
    | Num(x) -> "NUM(" + (string) x + ")"
    | Var(x) -> x
    | ArrEntry (a, b) -> a + "LBRACK" + evalA b + "RBRACK"
    | MultExpr(x,y) -> "MULT(" + evalA x + ", " + evalA y + ")"
    | DivExpr(x,y) -> "DIV(" + evalA x + ", " + evalA y + ")"
    | AddExpr(x,y) -> "ADD(" + evalA x + ", " + evalA y + ")"
    | MinusExpr(x,y) -> "MINUS(" + evalA x + ", " + evalA y + ")"
    | PowExpr(x,y) -> "POW(" + evalA x + ", " + evalA y + ")"
    | UMinusExpr(x) -> "MINUS(" + evalA x + ")"

(* 
let rec evalB e =
    match e with
    | B(x) -> x
    | AndExpr(x, y)             -> (evalB x) && (evalB y)
    | OrExpr(x, y)              -> evalB x || evalB y
    | ScAndExpr(x, y)           -> evalB x && evalB y
    | ScOrExpr(x, y)            -> evalB x || evalB y
    | NotExpr(x)                -> not (evalB x)
    | EqualExpr(x, y)           -> evalA x = evalA y
    | NotEqualExpr(x, y)        -> not (evalA x = evalA y)
    | GreaterThanExpr(x, y)     -> evalA x > evalA y
    | GreaterOrEqualExpr(x, y)  -> evalA x >= evalA y
    | LessThanExpr(x, y)        -> evalA x < evalA y
    | LessOrEqualExpr(x, y)     -> evalA x <= evalA y
*)
let rec evalB e =
    match e with
        | True -> "TRUE"
        | False -> "FALSE"
        | AndExpr(x, y)             -> "AND(" + evalB x + ", " + evalB y + ")"
        | OrExpr(x, y)              -> "OR(" + evalB x + ", " + evalB y + ")"
        | ScAndExpr(x, y)           -> "SCAND(" + evalB x + ", " + evalB y + ")"
        | ScOrExpr(x, y)            -> "SCOR(" + evalB x + ", " + evalB y + ")"
        | NotExpr(x)                -> "NOT(" + evalB x + ")"
        | EqualExpr(x, y)           -> "EQUALS(" + evalA x + ", " + evalA y + ")"
        | NotEqualExpr(x, y)        -> "NEQUALS(" + evalA x + ", " + evalA y + ")"
        | GreaterThanExpr(x, y)     -> "GREATER(" + evalA x + ", " + evalA y + ")"
        | GreaterOrEqualExpr(x, y)  -> "GOE(" + evalA x + ", " + evalA y + ")"
        | LessThanExpr(x, y)        -> "LESS(" + evalA x + ", " +  evalA y + ")"
        | LessOrEqualExpr(x, y)     -> "LOE(" + evalA x + ", " +  evalA y + ")"

(*
let rec evalC e =
    match e with
    | AssignExpr(str, a)            -> variablesArray@[(str, evalA a)]
    | AssignToArrExpr(arr, a, b)    -> variablesArray
    | SkipExpr                      -> variablesArray
    | DoubleExpr(a, b)              -> evalC a@evalC b
    | IfExpr(a)                     -> variablesArray
    | DoExpr(a)                     -> variablesArray
*)

let rec edgesGC q1 q2 commando =
    match (commando) with
    | AssignExpr (x,y) -> (q1,x + evalA y, q2)::[]
    | AssignToArrExpr (x,a,b) -> (q1, x + evalA a + evalA b, q2)::[]
    
    
and edgesC q1 q2 commando =
    match (commando) with
    |


let rec evalC e =
    match e with
        | AssignExpr(str, a)            -> "ASSIGN(" + str + ", " + evalA a + ")"
        | AssignToArrExpr(str, a, b)    -> "ASSIGN(" + str + "LBRACK" + evalA a + "RBRACK" + evalA b + ")"
        | SkipExpr                      -> "SKIP"
        | DoubleExpr(a, b)              -> "DOUBLE(" + evalC a + ", " + evalC b + ")"
        | IfExpr(a)                     -> "IF(" + evalGC a + "FI)"
        | DoExpr(a)                     -> "DO(" + evalGC a + "OD)"
and evalGC e =
    match e with
        | ArrowExpr(b, c)       -> "ARROW(" + evalB b + ", " + evalC c + ")"
        | AlsoExpr(a, b)        -> "ALSO(" + evalGC a + ", " + evalGC b + ")"

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Fm4FunParser.start Fm4FunLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: %s" (evalC(e))
        compute n
        with err -> printfn "Not a valid language"
                    compute (n-1)
                    

// Start interacting with the user
compute 3
