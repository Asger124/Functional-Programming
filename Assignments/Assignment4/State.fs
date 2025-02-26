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

    
    type state = 
    |S of Map<string, int>
    
    
    let mkState ()= S Map.empty 


    let random _ = failwith "not implemented"
    
    let declare x st = 
        match st with 
        |S map when (not(Map.containsKey x map)) && (not(reservedVariableName x)) && validVariableName x -> 
            let newmap = Map.add x 0 map 
            Some(S newmap)
            
        |_ -> None 
         
    let getVar x st = 
        match st with 
        |S map -> Map.tryFind x map 
    
    
    let setVar x v st =
        match st with 
        |S map when (Map.containsKey x map) -> let addv = Map.add x v map 
                                               Some (S addv)
        |_ -> None                              

                  
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     