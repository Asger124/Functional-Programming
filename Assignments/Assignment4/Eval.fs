module Interpreter.Eval

    open Result
    open Language
    open State
    
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
        |Var v -> getVar v st 
                  
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

    
    let rec stmntEval s (st:state) = 
        match s with 
        |Skip -> Some st 
        |Declare v -> declare v st  
        |Assign(v,a) -> let xval = arithEval a st
                        Option.bind(fun x -> setVar v x st) xval

        |Seq(s1,s2) -> let stmtval1 = stmntEval s1 st 
                       let stmntval2 = Option.bind(fun x -> stmntEval s2 x) stmtval1 
                       Option.bind(fun x -> Some(x))stmntval2 

        |If(guard,s1,s2) -> let booleval = boolEval guard st 
                            let stmtval1 = stmntEval s1 st 
                            let stmtval2 = stmntEval s2 st 
                            Option.bind(fun x -> Option.bind(fun y -> if x=true then Some(y) 
                                                                      else Option.bind (fun z -> Option.bind (fun g -> 
                                                                      if z=false then Some(g) else None) stmtval2) booleval) stmtval1) booleval       
        |While(guard,s') -> let booleval = boolEval guard st 
                           
                            let stmtval = stmntEval s' st 
                            let stopt = stmntEval Skip st 
                            Option.bind(fun x -> Option.bind(fun y -> if x=true then stmntEval s y  
                                                                      else Option.bind(fun z -> Option.bind(fun g -> 
                                                                      if z=false then Some(g) else None) stopt) booleval) stmtval) booleval 
                        
                         
                        
                        
                 
                 