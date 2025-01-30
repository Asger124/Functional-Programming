   let rec printValueNTimes n =
    match n with
    | 0 -> ()
    | _ -> printfn "Hipp hipp hurra!" 
           printValueNTimes (n - 1)
        
printValueNTimes 20