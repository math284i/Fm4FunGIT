open System.Reflection.Metadata.LocalScopeHandleCollection

#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "Fm4FunTypesAST.fs"
open Fm4FunTypesAST
#load "Fm4FunParser.fs"
open Fm4FunParser
#load "Fm4FunLexer.fs"
open Fm4FunLexer


//Global stuff
let variablesArray = []
let mutable globalQ = 0

let mutable currentNode = "▷"

type commando =
    | C of Cexpr
    | GC of GuardedExpr
    
let mutable programGraph = Map<string,Set<commando*string>>[];
let mutable status = "Terminated"
let mutable lastNode = "▷"

let addToProgramGraph q1 c q2 =
    if (Map.containsKey q1 programGraph) then
                                         let a = Map.find q1 programGraph
                                         let newSet = Set.add (c,q2) a
                                         Map.add q1 newSet programGraph
                                         else Map.add q1 (Set.empty.Add(c,q2)) programGraph


let rec variableHelper str arr =
    match arr with
    | []                     -> failwith "Undefined variable"
    | (x,y)::xs when x = str -> y
    | x::xs                  -> variableHelper str xs

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


let rec evalC e =
    match e with
        | AssignExpr(str, a)            -> str + ":=" + evalA a
        | AssignToArrExpr(str, a, b)    -> evalA (ArrEntry (str,a)) + ":=" + evalA b
        | SkipExpr                      -> "SKIP"
        | DoubleExpr(a, b)              -> evalC a + " ; " + evalC b
        | IfExpr(a)                     -> "if " + evalGC a + " fi"
        | DoExpr(a)                     -> "do " + evalGC a + " od)"
and evalGC e =
    match e with
        | ArrowExpr(b, c)       -> evalB b + "->" + evalC c
        | AlsoExpr(a, b)        -> evalGC a + " [] " + evalGC b
        
let mutable dom = Map<string,int>[]

let rec semA a =
    match a with
    | Var(x) when Map.containsKey x dom -> Map.find x dom
    | Num(x) -> x
    | AddExpr(x,y) -> semA x + semA y
    | MinusExpr(x,y) -> semA x - semA y
    | MultExpr(x,y) -> semA x * semA y
    | DivExpr(x, y) when semA y <> 0 -> semA x / semA y
    | UMinusExpr(x) -> semA x
    | PowExpr(x,y) -> pown (semA x) (semA y)
    | ArrEntry(x, y) -> 0 //TODO
    | x -> status = "Stuck"
           printfn "Undefined: %s", evalA x
           0


let rec semB b =
    match b with
    | True -> true
    | False -> false
    | EqualExpr(x,y) -> semA x = semA y 
    | NotEqualExpr(x,y) -> not (semA x = semA y)
    | GreaterThanExpr(x,y) -> semA x > semA y
    | GreaterOrEqualExpr(x,y) -> semA x >= semA y
    | LessThanExpr(x,y) -> semA x < semA y
    | LessOrEqualExpr(x,y) -> semA x <= semA y
    | AndExpr(x,y) -> let a = semB x
                      let b = semB y
                      a && b
    | OrExpr(x,y) -> let a = semB x
                     let b = semB y
                     a || b
    | ScAndExpr(x,y) -> semB x && semB y
    | ScOrExpr(x,y) -> semB x || semB y
    | NotExpr(x) -> not (semB x)

let rec semGC gc =
    match gc with
    | ArrowExpr(b, c) -> semB b
    | AlsoExpr(x, y)  -> failwith("Can't evaluate Also!")

and semC c =
    match c with
    | AssignExpr(x, y) when Map.containsKey x dom -> dom <- Map.add x (semA y) dom
                                                     true
    | AssignToArrExpr(x, y, z) -> true //TODO
    | SkipExpr  -> true
    | DoubleExpr(x, y) -> failwith("Can't evaluate Double!")
    | IfExpr(x) -> semGC x
    | DoExpr(x) -> semGC x
    | x -> status = "Stuck"
           printfn "Undefined: %s", evalC x
           false        
        

let rec doneGC gc =
    match (gc) with
    | ArrowExpr (b, c)  -> NotExpr b
    | AlsoExpr (a, b)   -> AndExpr (doneGC a, doneGC b)

let rec edgesCn q1 q2 commando =
    match (commando) with
    | AssignExpr (x,y)          -> (q1, evalC (AssignExpr (x, y)), q2)::[]
    | AssignToArrExpr (x,a,b)   -> (q1, evalC (AssignToArrExpr (x, a, b)), q2)::[]
    | SkipExpr                  -> (q1, evalC SkipExpr, q2)::[]
    | DoubleExpr (x, y)         -> globalQ <- globalQ + 1
                                   let E1 = edgesCn q1 (globalQ.ToString()) x
                                   let E2 = edgesCn (globalQ.ToString()) q2 y
                                   E1 @ E2
    | IfExpr (a)                -> edgesGCn q1 q2 a
    | DoExpr (a)                -> let b = doneGC a
                                   (q1, evalB b , q2)::edgesGCn q1 q1 a
    
and edgesGCn q1 q2 commando =
    match (commando) with
    | ArrowExpr (b, c)          -> globalQ <- globalQ + 1
                                   (q1, evalB b, (globalQ.ToString()))::edgesCn (globalQ.ToString()) q2 c
    | AlsoExpr (g1, g2)          -> let E1 = edgesGCn q1 q2 g1
                                    let E2 = edgesGCn q1 q2 g2
                                    E1 @ E2

let rec edgesCd q1 q2 commando =
    match (commando) with
    | AssignExpr (x,y)          -> programGraph <- addToProgramGraph q1 (C (AssignExpr (x, y))) q2
                                   (q1, evalC (AssignExpr (x, y)), q2)::[]
    | AssignToArrExpr (x,a,b)   -> programGraph <- addToProgramGraph q1 (C (AssignToArrExpr (x, a, b))) q2
                                   (q1, evalC (AssignToArrExpr (x, a, b)), q2)::[]
    | SkipExpr                  -> programGraph <- addToProgramGraph q1 (C SkipExpr) q2
                                   (q1, evalC SkipExpr, q2)::[]
    | DoubleExpr (x, y)         -> globalQ <- globalQ + 1
                                   let E1 = edgesCd q1 (globalQ.ToString()) x
                                   let E2 = edgesCd (globalQ.ToString()) q2 y
                                   E1 @ E2
    | IfExpr (a)                -> let (E,d) = edgesGCd q1 q2 a False
                                   E
    | DoExpr (a)                -> let b = doneGC a
                                   let (E,d) = edgesGCd q1 q1 a False
                                   //addToProgramGraph q1 ( semB (NotExpr d)) q2
                                   E@(q1, evalB (NotExpr d), q2)::[]
                                 
and edgesGCd q1 q2 commando d =
    match (commando) with
    | ArrowExpr (b, c)          -> globalQ <- globalQ + 1
                                   //addToProgramGraph q1 (semB (AndExpr(b, NotExpr d))) (globalQ.ToString())
                                   ((q1, evalB (AndExpr(b, NotExpr d)), (globalQ.ToString()))::edgesCd (globalQ.ToString()) q2 c, OrExpr(b,d))
    | AlsoExpr (g1, g2)         -> let (E1,d1) = edgesGCd q1 q2 g1 d
                                   let (E2,d2) = edgesGCd q1 q2 g2 d1
                                   (E1 @ E2, d2)


let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Fm4FunParser.start Fm4FunLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let rec printList = function
    | []            -> ""
    | (a,b,c)::xy         -> printfn "q%s -> q%s[label = \"%s\"];" a c b
                             printList xy

//printing the step-wise evaluation to the console
let rec printGCL = function
    | [] -> ""
    //| //<-- put stuff here


//We implement here the interpreter for the GCL
let rec interpreter n =
    printf "Enter initial values: "
    try
        let e = parse (Console.ReadLine())
        //printGCL //<-- put stuff here
        interpreter n
        with err -> printfn "some value(s) are not valid"
                    interpreter (n-1)


    
let parseCommandLine args e =
    match args with
    | "n" -> printList (edgesCn "▷" "◀" e)
    
    | "d" -> printList (edgesCd "▷" "◀" e)

let printerT3 stat last map =
    printfn "status: %s" stat
    printfn "Final Node: %s" lastNode
    let result = Map.fold (fun state key value -> key + ":" + string value + "\n") "" map
    //Alternativt (Hvis den øverste ikke virker):
    //Map.fold (fun state key value -> printfn "%s: %i" key value) () map
    printfn "%s" result

let rec evaluateProgramGraph node =
    let set = Set.toList (Map.find node programGraph)
    for item in set do
        let (c, s) = item
        match c with
        | C(x)    -> if semC x then evaluateProgramGraph s else
                                                               status <- "Stuck"
                                                               lastNode <- node
        | GC(x)   -> if semGC x then evaluateProgramGraph s else
                                                               status <- "Stuck"
                                                               lastNode <- node

let rec getInput str =
    printf str
    let input = Console.ReadLine()
    if input <> "no" then
        let key:string = string (input[0])
        let value:int = int input[3]
        dom <- Map.add key value dom
        getInput str

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        getInput "Enter a variable (x:=y) or no to continue: "
        printfn "Enter an expression: "
        try
        let e = parse (Console.ReadLine())
        semC e
        printerT3 status lastNode dom
        compute n
        with err -> printfn "Not a valid language"
                    compute (n-1)
        

// Start interacting with the user
compute 3
