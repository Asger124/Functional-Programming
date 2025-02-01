module Assignment1
    open System
    
    let sqr x = x*x

    let pow x n = System.Math.Pow(x,n)
    
    let rec fib n = 
        match n with 
        |0 -> 0
        |1 -> 1 
        |n -> fib (n-1) + fib (n-2)

    let rec sum n = 
        match n with 
        | 0 -> 0 
        | n -> n + sum(n-1);;


    let dup (s:string) = s + s

    let rec dupn (s:string) n =
        match n with
        |0 -> ""
        |_ -> s + dupn s (n-1)

    let rec bin (n,k)  = 
        match (n,k) with 
        |(n,0) -> 1
        |(n,k) when n = k -> 1
        |(n,k) -> bin(n-1,k-1) + bin(n-1,k)

    (*
    bin(4,2) = bin(3,1) + bin(3,2)
               Compute bin(3,2):
                     bin(3,2) = bin(2,1) + bin(2,2)
                                bin(2,2) = 1
                                bin(2,1) = bin(1,1) + bin(1,0) = 1 + 1 

               Compute bin(3,1)
                bin(3,1) = bin(2,0) + bin(2,1) = 1 + 2 

    results:
     bin(3,1) = 3 
     +
     bin (3,2) = 3 
     =
    bin(4,2) = 6 *)

    
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
        
                    
              

    let timediff (h1,m1) (h2,m2) = ((h2 - h1)*60) + ( m2 - m1)

    let minutes (h1,m1) = timediff (0,0) (h1,m1)

    let curry f a b = f (a,b)
    let uncurry f (a,b) =  f a b 