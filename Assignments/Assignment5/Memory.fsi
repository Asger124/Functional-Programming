module Interpreter.Memory

type memory 

val alloc: int -> memory -> (memory*int) option

val free: int -> int -> memory -> memory option

val setMem: int -> int -> memory -> memory option 

val getMem: int -> memory -> int option

val empty: int -> memory 