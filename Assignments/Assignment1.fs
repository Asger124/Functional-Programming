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


    let dup s = printfn "%s" (s + " " + s);;

    let rec dupn s n =
        match n with
        |0 -> ()
        |_ -> printf "%s" (s + " ") 
              dupn s (n-1)

    let rec bin (n,k)  = 
        match (n,k) with 
        |(n,0) -> 1
        |(n,k) when n = k -> 1
        |(n,k) -> bin(n-1,k-1) + bin(n-1,k)

    //bin(4,2) = bin(3,2) + (bin(3,1) == 1 + 1)
    //bin(3,2) = bin (2,2) + (bin(2,1) == 1 + 1))
    //bin(2,2) = bin(1,2) + (bin(1,1) == 1)

    
    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str
    
    let readInt() = failwith "not implemented"

    let timediff _ = failwith "not implemented"

    let minutes _ = failwith "not implemented"

    let curry _ = failwith "not implemented"
    let uncurry _ = failwith "not implemented"