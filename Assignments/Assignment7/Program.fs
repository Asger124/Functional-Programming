// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State

let runProgram prog =
    42 |>
    Some |>
    mkState 10 |>
    stmntEval prog |>
    ignore

// Uncomment the program you want to run

runProgram guessANumber
//runProgram bubbleSort



//For testing

//#load "Memory.fs"
//#load "Language.fs"
//#load "State.fs"
//#load "StateMonad.fs"
//#load "Eval.fs"


//open Interpreter.Memory
//open Interpreter.Language 
//open Interpreter.State
//open Interpreter.StateMonad
//open Interpreter.Eval