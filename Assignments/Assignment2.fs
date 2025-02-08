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


    let explode1 (s:string)  = s.ToCharArray() |> List.ofArray
    
    let rec explode2 (s:string) = 
        match s with 
        | "" -> []
        |s-> s.Chars(0)::explode2 (s.Remove(0,1));;

    let rec implode (cs:char list) : string = 
        match cs with 
        |[] -> ""
        |x::xs -> x.ToString() + implode xs 

    let rec implodeRev (cs:char list) = 
        match cs with 
        |[] -> ""
        |x::xs -> implodeRev xs + x.ToString()

        (*ImplodeRev H e l 
        
                implodeRev [e l] + H.tostring() =leh 
                    ImplodeRev [l] + e.toString() = l e
                        ImplodeRev [] + l.toString = l
                            "" 
                 *)

    let rec Iterator xs = 
        match xs with 
        |[] -> []
        |x::xs -> System.Char.ToUpper x :: Iterator xs;;

    let toUpper s = s |> explode1 |> Iterator |> implode;;


    (*
    Easier,but maybe not allowed,version? :
    
    let stringToUpper (s:string) = s.ToUpper();;
    *)

    let rec ack (m,n) = 
        match (m,n) with 
        |(m,n) when m < 0 || n < 0 -> failwith "Function only works on non negative numbers"
        |(0,n) -> n+1
        |(m,0) -> ack (m-1,1)
        |(m,n) -> ack(m-1,ack(m,n-1));;
