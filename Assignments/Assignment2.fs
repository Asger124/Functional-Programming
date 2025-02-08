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

    let rec removeOddIdx xs =
        match xs with 
        |[] -> []
        |x::_::xs -> x:: removeOddIdx xs
        |x::xs -> x::xs 

                  
    let rec combinePair xs = 
        match xs with 
        |[] -> []
        |x::x1::xs -> (x,x1)::combinePair xs 
        |x::xs -> []
        

    //Record type for complex:

    //type complex = {a:float; b:float}

    //let mkComplex (x:float) (y:float) : complex = (a=x,b=y)
    
    //let complexToPair (a:complex)  = (a.a,a.b)


    type complex =  (float*float);;

    let mkComplex (a:float) (b:float) : complex = (a,b);;
    

    let complexToPair (a:complex) = (fst a,snd a);;



    let (|+|) (a:complex) (b:complex) : complex = (fst a + fst b,snd a+snd b);;


    let (|*|) (a:complex) (b:complex) : complex = (fst a * fst b) - (snd a * snd b), (snd a * fst b) + (fst a * snd b);;

    let (|-|) (a:complex) (b:complex) : complex =  (fst a - fst b, snd a - snd b);; 

    let (|/|) (a:complex) (b:complex) : complex = a |*| mkComplex  (fst b / ( fst b**2 + snd b**2.)) ((-snd b / (fst b**2 + snd b**2)));;


    let explode1 _ = failwith "not implemented"

    let rec explode2 _ = failwith "not implemented"

    let implode _ = failwith "not implemented"
    let implodeRev _ = failwith "not implemented"

    let toUpper _ = failwith "not implemented"

    let ack _ = failwith "not implemented"