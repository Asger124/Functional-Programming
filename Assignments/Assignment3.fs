module Assignment3
    
    let add5 x = 5 + x;;
    let mul3 x = x * 3;;

    let add5mul3 x = x |> add5  |> mul3;; 
    let add5mul3_2  = add5  >> mul3;;
    
    let add5_2 f a = f a |> add5;;
    let mul3_2 f a = f a |> mul3;;
   
    // with pipes:
    
    let rec downto4 f n e = 
        match n with 
        |n when n <= 0 -> e  
        |n ->  (f n e) |> downto4 f (n-1)

    
    (* Without: 

    let rec downto4 f n e = 
        match n with 
        |n when n <= 0 -> e  
        |n ->  downto4 f (n-1) (f n e)
        *)
    

    let fac n = 
        match n with 
        |n when n=0 -> 1 
        |n -> downto4 (fun acc x -> x*acc) n 1

    (*
    build a list with downto4. Construct a new list where g has been applied to all elements of list.
    Done by sending the constructed list to List.map with piping
    *)

    let range g n = 
        match n with 
        |n when n<0 -> []
        |n -> downto4 (fun x acc -> x::acc) n [] |> List.map g 

    (*
    Alternative version of the range function. 
    Apply function g to each element before it is added to the constructed list.

    let AlternativeRange g n = 
        match n with 
        |n when n<0 -> []
        |n -> downto4 (fun x acc -> (g x)::acc) n [] 
    *)
    
    let rec double xs  = 
        match xs with 
        |[] -> []
        |x::xs -> (x*2)::double xs 
        
    let double_2 lst = List.map (fun x -> x*2) lst 
    
    let rec stringLength (xs:string list) =
        match xs with 
        |[] -> []
        |x::xs -> x.Length::stringLength xs 
        
    let stringLength_2 lst = List.map (fun (x:string) -> x.Length) lst 
    
    let rec keepEven xs =
        match xs with 
        |[] -> []
        |x::xs when x%2=0 -> x::keepEven xs
        |x::xs -> keepEven xs 
    
    let keepEven_2 lst = List.filter (fun x ->x%2=0) lst 
    
    let rec keepLengthGT5 (xs:string list) =
        match xs with 
        |[] -> []
        |x::xs when x.Length > 5 -> x::keepLengthGT5 xs 
        |x::xs -> keepLengthGT5 xs 
        
    let keepLengthGT5_2 lst = List.filter (fun (x:string) -> x.Length>5) lst
    
    
    let rec sumPositive xs =
        match xs with 
        |[] -> 0
        |x::xs when x >=0 -> x + sumPositive xs 
        |x::xs -> sumPositive xs 
        
    let rec sumPositive_2 lst = List.fold (fun acc elem -> if elem >=0 then acc + elem else acc ) 0 lst
        
    let rec sumPositive_3 lst = List.filter (fun x -> x>=0) lst |> List.fold (fun acc x -> acc+x) 0 
        
  
    let add5mul3_3 _ = failwith "not implemented"
    
 
    let rec mergeFuns _ = failwith "not implemented"
        
    let rec facFuns _ = failwith "not implemented"
        
    let fac_2 _ = failwith "not implemented"

    let removeOddIdx _= failwith "not implemented"
        
    
    let weird _ = failwith "not implemented"
    
   
    let insert _= failwith "not implemented"
                
    let rec permutations _ = failwith "not implemented"