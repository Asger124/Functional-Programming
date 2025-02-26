module Interpreter.Eval

    open Result
    open Language
    open State

    let stmntEval _ = failwith "not implemented"
    
    //For arithEval I first used Option.Get to extract the integer values needed for the arithmetic operations,
    //for example match on Mod looked like this:
     //let x = arithEval b st |> Option.get 
     //let y = arithEval c st |> Option.get 
     //                 if y > 0 then Some(x%y) 
     //                 else None 

    //But after seeing an example in the book, i changed it to matching on the values evaluated from recursive calls.
    //I think this is better as i dont need to make an explicit If Else statement to return Some or None 
    // - as the pattern match effectively does that. Also i think Option.get can throw an exeption
    //breaking the proper handling of returning a None value, if no match is found, but i not entirely sure.

    let rec arithEval a st  = 
        match a with 
        |Num x -> Some x
        |Var v  -> match st with 
                   |S map -> Map.tryFind v map 
                    
        |Add(b,c) -> match(arithEval b st, arithEval c st) with 
                     |Some(x), Some(y) -> Some(x+y) 
                     |_ -> None 

        |Mul(b,c) -> match(arithEval b st, arithEval c st) with 
                     |Some(x), Some(y) -> Some(x*y) 
                     |_ -> None
        
        |Div(b,c) ->  match (arithEval b st, arithEval c st) with 
                      |Some(x),Some (y) when y > 0  -> Some (x/y) 
                      |_ -> None 

        |Mod(b,c) ->  match(arithEval b st, arithEval c st) with 
                      |Some(x),Some(y) when y > 0 -> Some(x%y) 
                      |_ -> None 
  

    //Credit to the Livecoding sessions helping me realize how Option.bind can be chained together to handle more arguments.
  
    let rec arithEval2 a st  = 
        match a with 
        |Num x -> Some x 
        |Var v -> match st with 
                  |S map -> Map.tryFind v map
                  
        |Add(b,c) -> let xval = arithEval2 b st
                     let yval = arithEval2 c st
                     Option.bind(fun x -> Option.bind (fun y -> Some(x+y)) yval) xval

        |Mul(b,c) -> let xval = arithEval2 b st
                     let yval = arithEval2 c st
                     Option.bind(fun x -> Option.bind (fun y -> Some(x*y)) yval) xval

        |Div(b,c) -> let xval = arithEval2 b st
                     let yval = arithEval2 c st
                     Option.bind (fun x -> Option.bind(fun y -> if y > 0 then Some(x/y) else None) yval) xval

        |Mod(b,c) -> let xval = arithEval2 b st
                     let yval = arithEval2 c st
                     Option.bind(fun x -> Option.bind (fun y -> if y > 0 then Some(x%y) else None) yval) xval
      
    let rec boolEval b st =
       match b with 
       |TT -> Some true
       |Eq(a,c) ->  let xval = arithEval2 a st
                    let yval = arithEval2 c st
                    Option.bind(fun x -> Option.bind(fun y -> Some(x=y)) yval) xval
        
       |Lt(a,c) -> let xval = arithEval2 a st
                   let yval = arithEval2 c st
                   Option.bind(fun x -> Option.bind(fun y -> Some(x>y)) xval) yval
                    
       |Conj(a,c) -> let xval = boolEval a st
                     let yval = boolEval c st
                     Option.bind(fun x -> Option.bind(fun y -> Some(x&&y)) yval) xval
       |Not a -> let xval = boolEval a st 
                 Option.bind(fun x -> Some(not x)) xval
                 
                 
                 