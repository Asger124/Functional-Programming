module Interpreter.State
    open Interpreter.Memory

    
    let validVariableName v = 
        match v with 
        |v when System.Char.IsLetter (v,0) || v.Chars(0) = '_' -> true 
        |v when String.forall System.Char.IsLetterOrDigit v -> true 
        |v when String.forall (fun x -> x='_') v  -> true 
        |_ -> false 


    let varList = ["if"; "then"; "else";"while";"declare";"print";"random";"fork";"__result__"]

    let reservedVariableName v = List.exists (fun x -> x=v) varList 

    
    type state = { S: Map<string, int>; memory:memory; Rng: System.Random}

    
    let mkState memsize oseed = {S = Map.empty; memory = Memory.empty memsize; 
        Rng = match oseed with 
                |Some seed -> System.Random(seed) 
                |None -> System.Random()} 


    let random st = 
        match st with 
        |{state.Rng = rng} -> rng.Next() 
    
    let declare x st = 
        match st with 
        |{state.S = map; state.memory=mem; state.Rng=rng} when (not(Map.containsKey x map)) && (not(reservedVariableName x)) && validVariableName x -> let newmap = Map.add x 0 map 
                                                                                                                                                       Some({state.S = newmap; state.memory = mem; state.Rng=rng})
            
        |_ -> None 
         
    let getVar x st = 
        match st with 
        |{state.S = map} -> Map.tryFind x map 
    
    
    let setVar x v st =
        match st with 
        |{state.S = map; state.memory = mem} when (Map.containsKey x map) -> 
                                                                        let addv = Map.add x v map 
                                                                        Some({state.S = addv; state.memory = mem; state.Rng = st.Rng})
        |_ -> None          
     
    let alloc x size st =
        match st with 
        |{state.S = map; state.memory = mem} -> let newmemory = Memory.alloc size mem 
                                                let memoryvalue = match newmemory with 
                                                                  |Some x -> x 
                                                                  |_ -> (Memory.empty size,0)
                                            //let memoryvalue = Option.get newmemory
                                                let ptr = snd(memoryvalue) 
                                                let mem = fst(memoryvalue)
                                                let newstate = setVar x ptr st 
                                                match newstate with 
                                                |Some x -> Some({state.S = x.S; state.memory = mem; state.Rng = st.Rng})
                                                |_ -> None  
                                                


    let free (ptr:int) (size:int) (st:state) =
        match st with 
        |{state.memory = mem} -> let freemem = Memory.free ptr size mem 
                                 //let memval = Option.get freemem 
                                 match freemem with 
                                 |Some x -> Some{state.S = st.S; state.memory = x; state.Rng = st.Rng} 
                                 |None -> None
                                 

    let setMem (ptr:int) (v:int) (st:state) = 
        match st with 
        |{state.memory = mem} -> let setter = Memory.setMem ptr v mem 
                                 //let setval = Option.get setter 
                                 match setter with 
                                 |Some x -> Some{state.S = st.S; state.memory = x; state.Rng = st.Rng}
                                 |None -> None 

                                 

    let getMem (ptr:int) (st:state) = 
        match st with 
        |{state.memory = mem} ->  Memory.getMem ptr mem 
                                 


                  
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     


  

