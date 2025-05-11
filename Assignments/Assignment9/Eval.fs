module Interpreter.Eval

    open Result
    open Language
    open StateMonad

    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str

    let rec readInt() = 
                let input = readFromConsole()
                let (bool,res) = tryParseInt input
                if bool then  
                     res 
                else 
                    printfn "%s is not an integer" input
                    readInt ()
    


    type StateBuilder() =  
        member this.Bind(f, x) = (>>=) f x  
        member this.Return(x) = ret x  
        member this.ReturnFrom(x) = x  
        member this.Combine(a, b) = a >>= (fun _ -> b) 
      
    let eval = StateBuilder()


    let rec arithEval (a:aexpr) = 
        match a with 
        |Num x -> ret x  
        |Var v -> getVar v 
                  
        |Add(b,c) -> let xval = arithEval b 
                     let yval = arithEval c 
                     (>>=) xval (fun x -> (>>=) yval (fun y -> ret(x+y)))        

        |Mul(b,c) -> let xval = arithEval b 
                     let yval = arithEval c 
                     (>>=) xval (fun x -> (>>=) yval (fun y -> ret(x*y))) 

        |Div(b,c) -> let xval = arithEval b 
                     let yval = arithEval c 
                     (>>=) xval (fun x -> (>>=) yval (fun y -> if y > 0 then ret (x/y) else fail))

        |Mod(b,c) -> let xval = arithEval b
                     let yval = arithEval c
                     (>>=) xval (fun x -> (>>=) yval (fun y -> if y > 0 then ret(x%y) else fail)) 
        //Some v if a is equal to MemRead e1, e1 evaluates to Some ptr and ptr points to v in the memory of the state.

        |MemRead e1 ->  let xval = arithEval e1 
                        (>>=) xval (fun x -> getMem x) 

        |Random -> random  

        |Read -> let read = readInt ()  
                 ret read   

        |Cond(b,a1,a2) -> let bval = boolEval b  
                          let a1val () = arithEval a1 
                          let a2val () = arithEval a2 
                          (>>=) bval (fun x -> if x then a1val ()  else a2val ())
                          
    and boolEval (b:bexpr): (bool stateMonad) =
       match b with 
       |TT -> ret true
       |Eq(a,c) ->  let xval = arithEval a 
                    let yval = arithEval c 
                    (>>=) xval (fun x -> (>>=) yval (fun y -> ret(x=y)))
        
       |Lt(a,c) -> let xval = arithEval a 
                   let yval = arithEval c 
                   (>>=) xval (fun x -> (>>=) yval (fun y -> ret(y>x)))
                    
       |Conj(a,c) -> let xval = boolEval a 
                     let yval = boolEval c 
                     (>>=) xval (fun x -> (>>=) yval (fun y -> ret(x&&y)))
       |Not a -> let xval = boolEval a  
                 (>>=) xval (fun x -> ret (not x))




    let rec arithEval2 (a:aexpr) = eval {
        match a with 
        |Num x -> return x  
        |Var v -> return! getVar v 
                  
        |Add(b,c) ->  let! x = arithEval2 b 
                      let! y = arithEval2 c
                      return x+y
                            

        |Mul(b,c) -> let! x = arithEval2 b 
                     let! y = arithEval2 c
                     return x*y
                          

        |Div(b,c) -> let! x = arithEval2 b 
                     let! y = arithEval2 c
                     if y <> 0 then
                      return x/y
                     else 
                      return! fail  
                          

        |Mod(b,c) -> let! x = arithEval2 b 
                     let! y = arithEval2 c
                     if y <> 0 then
                      return x/y
                     else 
                      return! fail 
 
        //Some v if a is equal to MemRead e1, e1 evaluates to Some ptr and ptr points to v in the memory of the state.

        |MemRead e1 ->  let! x = arithEval2 e1 
                        return! getMem x  

        |Random -> return! random  

        |Read -> let read = readInt ()  
                 return read   

        |Cond(b,a1,a2) -> let! bval = boolEval2 b  
                          let a1val () = arithEval2 a1 
                          let a2val () = arithEval2 a2 
                          if bval then 
                            return! a1val () 
                          else 
                            return! a2val ()
        }
                          
    and boolEval2 (b:bexpr): (bool stateMonad) = eval {
       match b with 
       |TT -> return true
       |Eq(a,c) ->  let! xval = arithEval2 a 
                    let! yval = arithEval2 c 
                    return xval=yval
        
       |Lt(a,c) -> let! xval = arithEval2 a 
                   let! yval = arithEval2 c 
                   return yval>xval
                    
       |Conj(a,c) -> let! xval = boolEval2 a 
                     let! yval = boolEval2 c 
                     return xval&&yval
       |Not a -> let! xval = boolEval2 a  
                 return not xval

    }

    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList

    let optPrint opt = 
        match opt with 
        |Some opt ->  printfn "%A" opt
        |None -> printfn "No value"


   
    let mergeStrings2 es s =
        let splitList = split s "%"
        let rec aux es splitList  (c: string -> 'a) =
             match es,splitList with 
             |[],[] -> c "" |> ret 
             |[],[ys] -> c ys |> ret 
             |[xs],[ys] -> fail  
             |x::xs, y::ys -> 
                    let str = arithEval x
                    (>>=) str (fun x -> 
                    aux xs ys  (fun r -> c(y+string(x)+r)))
                    
             |_ -> fail
               
        aux es splitList id


    let rec stmntEval s  = eval {
        match s with 
        |Skip -> return ()  
        |Declare v -> return! declare v
        |Assign(v,a) -> let! xval = arithEval2 a 
                        return! setVar v xval

        |Seq(s1,s2) -> do! stmntEval s1    
                       let! y = stmntEval s2 
                       return y 

        |If(guard,s1,s2) -> let! boolval = boolEval guard 
                            let! stmtval1 = stmntEval s1 
                            let! stmtval2 = stmntEval s2 
                            if boolval then 
                                return stmtval1 
                            else return stmtval2 

        |While(guard,s') -> let! boolval = boolEval guard
                                
                            if boolval then
                             do! stmntEval s'
                             do! stmntEval s
                            else 
                             return! stmntEval Skip 
                             


        |Alloc(x,e) -> let! size = arithEval e 
                       return! alloc x size 

        |MemWrite(e1,e2) -> let! ptr = arithEval e1
                            let! v = arithEval e2 
                            return! setMem ptr v 

        |Free(e1,e2) -> let! ptr = arithEval e1 
                        let! size = arithEval e2 
                        return! free ptr size 

        |Print(es,s) -> let! res = mergeStrings2 es s
                        printfn "%A" res
                        return ()
                        
        }            
                       

    
                        

    