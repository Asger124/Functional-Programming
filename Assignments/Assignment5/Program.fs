open Interpreter.Language

let [<EntryPoint>] main _ =
    printfn "%A" (Num 42)
    0


//For testing

//#load "Memory.fs"
//#load "State.fs"
//#load "Language.fs"  
//#load "Eval.fs" 


//open Interpreter.Memory
//open Interpreter.State
//open Interpreter.Language 
//open Interpreter.Eval