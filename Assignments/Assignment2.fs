module Assignment2

    let rec downto1 n = if n > 0 then n::downto1 (n-1)
                        else []
                      
    
    let rec downto2 n = 
        match n with 
        |n when n <= 0 -> []
        |n -> n::downto2 (n-1)

    let rec downto3 = 
        function 
        |n when n <= 0 -> []
        |n -> n::downto3 (n-1)

    let removeOddIdx _ = failwith "not implemented"

    let combinePair _ = failwith "not implemented"


    type complex = unit // Fill in your type here
    let mkComplex _ = failwith "not implemented"
    let complexToPair _ = failwith "not implemented"
    let (|+|) _ = failwith "not implemented"
    let (|*|) _ = failwith "not implemented"
    let (|-|) _ = failwith "not implemented"
    let (|/|) _ = failwith "not implemented"

    let explode1 _ = failwith "not implemented"

    let rec explode2 _ = failwith "not implemented"

    let implode _ = failwith "not implemented"
    let implodeRev _ = failwith "not implemented"

    let toUpper _ = failwith "not implemented"

    let ack _ = failwith "not implemented"