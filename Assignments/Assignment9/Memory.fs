module Interpreter.Memory


    open Interpreter.Language

        module oldMemory = 


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

    
     type message = 
        |Alloc of int *  AsyncReplyChannel<int * oldMemory.memory option> 
        |Free of int * int * AsyncReplyChannel<oldMemory.memory option> 
        |SetMem of int * int * AsyncReplyChannel<oldMemory.memory option>
        |GetMem of int * AsyncReplyChannel<int option>
        |Getch of AsyncReplyChannel<oldMemory.memory>

      type memory = Mem of MailboxProcessor<message>



    let inbox s (i : MailboxProcessor<message>) =

      let rec messageLoop (mem : oldMemory.memory) =async { 
                let! msg = i.Receive ()
                match msg with 
                |Alloc (x,reply) -> let update = oldMemory.alloc x mem 
                                    match update with 
                                    |Some x -> return! messageLoop (fst x) 
                                    |None -> return! messageLoop(mem)
                |Free (x,y,reply) -> 
                            let update = oldMemory.free x y mem 
                            match update with 
                            |Some x -> return! messageLoop x 
                            |None -> return! messageLoop mem 

                |SetMem (x,y,reply) -> 
                    let update = oldMemory.setMem x y mem
                    match update with 
                    |Some x -> return! messageLoop x 
                    |None -> return! messageLoop mem 

                |GetMem(x,reply) -> 
                    let update = oldMemory.getMem x mem
                    match update with 
                    |Some x -> return! messageLoop (mem)  
                    |None -> return! messageLoop(mem)
                |Getch ch -> ch.Reply(mem)   
                }
      messageLoop (oldMemory.empty s)
    let empty s = Mem (MailboxProcessor.Start (inbox s))

    let alloc size (Mem mp) = 
        let ptr, memOpt = mp.PostAndReply(fun reply -> Alloc(size, reply))
        match memOpt with
        | Some mem -> Some (mem, ptr)
        | None -> None


    let free ptr size (Mem mp) = mp.PostAndReply(fun reply -> Free(ptr,size,reply))
    let setMem ptr v (Mem mp) = mp.PostAndReply(fun reply -> SetMem(ptr,v,reply))
    let getMem ptr (Mem mp) = mp.PostAndReply(fun reply -> GetMem(ptr,reply))
                                    

    


    

    


