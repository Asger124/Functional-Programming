module Interpreter.State

    open Result
    open Language
    
    let validVariableName v = 
        match v with 
        |v when System.Char.IsLetter (v,0) || v.Chars(0) = '_' -> true 
        |v when String.forall System.Char.IsLetterOrDigit v -> true 
        |v when String.forall (fun x -> x='_') v  -> true 
        |_ -> false 


    let varList = ["if"; "then"; "else";"while";"declare";"print";"random";"fork";"__result__"]

    let reservedVariableName v = List.exists (fun x -> x=v) varList 

    
    type state = unit // your type goes here
    
    let mkState _ = failwith "not implemented"
    let random _ = failwith "not implemented"
    
    let declare _ = failwith "not implemented"
    
    let getVar _ = failwith "not implemented"
    let setVar _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     