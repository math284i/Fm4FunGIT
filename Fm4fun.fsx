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
let mutable globalQ = 0
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
    | Num(x) -> (string) x
    | Var(x) -> x
    | ArrEntry (a, b) -> a + "["  + evalA b + "]"
    | MultExpr(x,y) -> evalA x + "*" + evalA y
    | DivExpr(x,y) -> evalA x + "/" + evalA y
    | AddExpr(x,y) -> evalA x + "+" + evalA y
    | MinusExpr(x,y) -> evalA x + "-" + evalA y
    | PowExpr(x,y) -> evalA x + "^" + evalA y
    | UMinusExpr(x) -> "-" + evalA x

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
        | AndExpr(x, y)             -> "(" + evalB x + "&" + evalB y + ")"
        | OrExpr(x, y)              -> "(" + evalB x + "|" + evalB y + ")"
        | ScAndExpr(x, y)           -> "(" + evalB x + "&&" + evalB y + ")"
        | ScOrExpr(x, y)            -> "(" + evalB x + "||" + evalB y + ")"
        | NotExpr(x)                -> "(¬" + evalB x + ")"
        | EqualExpr(x, y)           -> "(" + evalA x + "=" + evalA y + ")"
        | NotEqualExpr(x, y)        -> "(" + evalA x + "!=" + evalA y + ")"
        | GreaterThanExpr(x, y)     -> "(" + evalA x + ">" + evalA y + ")"
        | GreaterOrEqualExpr(x, y)  -> "(" + evalA x + ">=" + evalA y + ")"
        | LessThanExpr(x, y)        -> "(" + evalA x + "<" +  evalA y + ")"
        | LessOrEqualExpr(x, y)     -> "(" + evalA x + "<=" +  evalA y + ")"

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


let rec evalC e =
    match e with
        | AssignExpr(str, a)            -> str + ":=" + evalA a
        | AssignToArrExpr(str, a, b)    -> str + "[" + evalA a + "]:=" + evalA b
        | SkipExpr                      -> "SKIP"
        | DoubleExpr(a, b)              -> evalC a + " ; " + evalC b
        | IfExpr(a)                     -> "if " + evalGC a + " fi"
        | DoExpr(a)                     -> "do " + evalGC a + " od)"
and evalGC e =
    match e with
        | ArrowExpr(b, c)       -> evalB b + "->" + evalC c
        | AlsoExpr(a, b)        -> evalGC a + " [] " + evalGC b

let rec doneGC GC =
    match (GC) with
    | ArrowExpr (b, c)  -> NotExpr b
    | AlsoExpr (a, b)   -> AndExpr (doneGC a, doneGC b)

let rec edgesC q1 q2 commando =
    match (commando) with
    | AssignExpr (x,y)          -> (q1, x + evalA y, q2)::[]
    | AssignToArrExpr (x,a,b)   -> (q1, x + evalA a + evalA b, q2)::[]
    | SkipExpr                  -> (q1, "skip", q2)::[]
    | DoubleExpr (x, y)         -> globalQ <- globalQ + 1
                                   let E1 = edgesC q1 globalQ x
                                   let E2 = edgesC globalQ q2 y
                                   E1 @ E2
    | IfExpr (a)                -> edgesGC q1 q2 a
    | DoExpr (a)                -> let b = doneGC a
                                   let E = edgesGC q1 q1 a
                                   E@(q1, evalB b , q2)::[]
    
and edgesGC q1 q2 commando =
    match (commando) with
    | ArrowExpr (b, c)          -> globalQ <- globalQ + 1
                                   let E = edgesC globalQ q2 c
                                   (q1, evalB b, globalQ)::E
    | AlsoExpr (g1, g2)          -> let E1 = edgesGC q1 q2 g1
                                    let E2 = edgesGC q1 q2 g2
                                    E1 @ E2

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Fm4FunParser.start Fm4FunLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let rec printList = function
    | []            -> ""
    | (a,b,c)::xy         -> printfn "q%i -> %s -> q%i" a b c
                             printList xy
                             
//printing the step-wise evaluation to the console
let rec printGCL = function
    | [] -> ""
    | //<-- put stuff here


//We implement here the interpreter for the GCL
let rec interpreter n =
    printf "Enter initial values: "
    try
        let e = parse (Console.ReadLine())
        printGCL() //<-- put stuff here
        interpreter n
        with err -> printfn "some value(s) are not valid"
                    interpreter (n-1)



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
        //printfn "Result: %s" (edgesC 0 -1 e)
        printList (edgesC 0 -1 e)
        interpreter 1
        compute n
        with err -> printfn "Not a valid language"
                    compute (n-1)
        

// Start interacting with the user
compute 5

