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
    
let rec evalC e =
    match e with
    | AssignExpr(str, a)            -> variablesArray@[(str, evalA a)]
    | AssignToArrExpr(arr, a, b)    -> variablesArray
    | SkipExpr                      -> variablesArray
    | DoubleExpr(a, b)              -> evalC a@evalC b
    | IfExpr(a)                     -> variablesArray
    | DoExpr(a)                     -> variablesArray

let rec evalGC e =
    match e with
    | ArrowExpr(b, c)       -> if (evalB b) then evalC c else variablesArray
    | AlsoExpr(a, b)        -> evalGC a@evalGC b

// We
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
        printfn "Result: %f" (evalA(e))
        compute n
        with err -> compute (n-1)

// Start interacting with the user
compute 3
