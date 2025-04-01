 module Interpreter.State
 

     type state 

     val random : state -> int 

     val mkState : int -> int Option ->  state 

     val declare : string -> state -> state option
 
     val setVar : string -> int -> state -> state option

     val getVar : string -> state -> int option

     val alloc : string -> int -> state -> state option

     val free : int -> int -> state -> state option

     val setMem : int -> int -> state -> state option

     val getMem : int -> state -> int option
    

 





    
    



