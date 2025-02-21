module Interpreter.Eval

    open Result
    open Language
    open State
    
    let rec arithEval (a:aexpr) = 
        match a with 
        |Num x -> Some x
        |Add(b,c) -> let x = arithEval b |> Option.get
                     let y = arithEval c |> Option.get
                     Some(x+y) 
        |Mul(b,c) -> let x = arithEval b |> Option.get 
                     let y = arithEval c |> Option.get 
                     Some(x*y)
        
        |Div(b,c) ->  match (arithEval b, arithEval c) with 
                      |Some(x),Some (y) when y > 0  -> Some (x/y) 
                      |_ -> None 
        |Mod(b,c) ->  let x = arithEval b |> Option.get 
                      let y = arithEval c |> Option.get 
                      if y > 0 then Some(x%y) 
                      else None 
  
    let rec arithEval2 (a:aexpr) = 
        match a with 
        |Num x -> Some x 
        |Add(b,c) -> let xval = arithEval2 b 
                     let yval = arithEval2 c 
                     Option.bind(fun x -> Option.bind (fun y -> Some(x+y)) xval) yval

        |Mul(b,c) -> let xval = arithEval2 b 
                     let yval = arithEval2 c 
                     Option.bind(fun x -> Option.bind (fun y -> Some(x*y)) xval) yval

        |Div(b,c) -> let xval = arithEval2 b 
                     let yval = arithEval2 c 
                     Option.bind (fun x -> Option.bind(fun y -> if y > 0 then Some(x+y) else None) xval) yval

        |Mod(b,c) -> let xval = arithEval2 b 
                     let yval = arithEval2 c 
                     Option.bind(fun x -> Option.bind (fun y -> if y > 0 then Some(x%y) else None) xval) yval
      
    let rec boolEval (b:bexpr) =
       match b with 
       |TT -> Some true
       |Eq(a,c) ->  let xval = arithEval2 a 
                    let yval = arithEval2 c
                    Option.bind(fun x -> Option.bind(fun y -> Some(x=y)) xval) yval
        
       |Lt(a,c) -> let xval = arithEval2 a 
                   let yval = arithEval2 c
                   Option.bind(fun x -> Option.bind(fun y -> Some(x>y)) xval) yval
                    
       |Conj(a,c) -> let xval = boolEval a 
                     let yval = boolEval c 
                     Option.bind(fun x -> Option.bind(fun y -> Some(x&&y)) xval) yval
       |Not a -> let xval = boolEval a
                 Option.bind (fun x -> Option.bind (fun y -> Some(x y) Not) xval
                 
                 
                 