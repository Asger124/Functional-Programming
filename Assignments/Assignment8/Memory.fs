module Interpreter.Memory
    
    //type adress = int 
    //type value = int

    type memory = {MemMap: Map<int,int>;
                   Next:int} 

    let empty (memsize:int) = {MemMap = Map.empty;
                         Next = 0}
                                           
                                                
    let alloc size (mem:memory) = 
        match mem with 
        |{memory.MemMap = map} when size > 0 -> let arr = Array.init size (fun x -> (x,0)) 
                                                let currentarr = Map.toArray map 
                                                let merge = Array.append currentarr arr 
                                                let newmap = Map.ofArray merge
                                                Some ({MemMap = newmap; Next = mem.Next + size}, mem.Next)
        |_ -> None 

   
      
    let free ptr size (mem:memory) = 
        match mem with 
        |{memory.MemMap = map} ->  let upperbound = ptr+size-1 
                                   let adressarr = [| ptr .. upperbound|]
                                   let keyscheck = Array.forall (fun key -> Map.containsKey key map) adressarr 
                                   let filterMap = Map.filter(fun addr _ -> addr < ptr || addr > upperbound) map
                                   if (keyscheck) then 
                                    Some {MemMap = filterMap; Next = mem.Next}
                                   else None 
        
        
    let setMem ptr v mem = 
        match mem with 
        |{memory.MemMap = map} when Map.containsKey ptr map ->let newmap = Map.add ptr v map
                                                              Some {MemMap = newmap; Next = mem.Next}
        |_ -> None 
        
    let getMem ptr mem =
        match mem with 
        |{memory.MemMap = map} when Map.containsKey ptr map -> Map.tryFind ptr map 
        |_ -> None 