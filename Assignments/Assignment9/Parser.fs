module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    //open JParsec.TextParser             // Example parser combinator library.
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    

    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc    : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "do"
    let pdeclare  : Parser<string> = pstring "declare"
    let ptrue     : Parser<string> = pstring "true"
    let pfalse    : Parser<string> = pstring "false"
    let pprint    : Parser<string> = pstring "print"
    let prandom   : Parser<string> = pstring "random"
    let pread     : Parser<string> = pstring "read"
    let pfunction : Parser<string> = pstring "not implemented"
    let pret      : Parser<string> = pstring "not implemented"
    
    let pwhitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace x) 
    let pletter         = satisfy (fun c -> System.Char.IsLetter c) 
    let palphanumeric   = satisfy(fun x -> System.Char.IsLetterOrDigit x)

    let spaces         = many pwhitespaceChar
    let spaces1        = many1 pwhitespaceChar

    let (.>*>.) p1 p2  = (p1 .>> spaces) .>>. p2 

    let (.>*>) p1 p2  = p1 .>> spaces .>> p2 

    let (>*>.) p1 p2 = p1 .>> spaces >>. p2 

    let parenthesise p = let openP = pchar '('  
                         let closeP = pchar ')'
                         let firstParse = (>*>.) openP p 
                         (.>*>) firstParse closeP

    let brackets p = let openP = pchar '{'  
                     let closeP = pchar '}'
                     let firstParse = (>*>.) openP p 
                     (.>*>) firstParse closeP

    let squares p = let openP = pchar '['  
                    let closeP = pchar ']'
                    let firstParse = (>*>.) openP p 
                    (.>*>) firstParse closeP

    let charConverter chars = List.fold(fun acc x -> acc + string x) "" chars

    let parseString= let charlist = many (satisfy (fun x -> System.Char.IsAscii x &&  x <> '"' ))
                     let strWithQuotes = charlist|>> (fun x ->  charConverter x)
                     let openQ = pchar '"'
                     let closeQ = pchar '"'
                     let firstParse = (>*>.) openQ strWithQuotes
                     (.>*>) firstParse closeQ

    let pid = (pletter <|> pchar '_') .>>. (many (palphanumeric <|> pchar '_')) |>> fun(x,y) ->  string x + charConverter y
    
    let unop op a = (>*>.) op a 
    let binop op a b =  a .>*> op .>*>. b

    let B2, b2ref = createParserForwardedToRef<bexpr>()
    let B1,b1ref = createParserForwardedToRef<bexpr>()
    let A1,a1ref = createParserForwardedToRef<aexpr>()
    let A2, a2ref = createParserForwardedToRef<aexpr>()
    let A3, a3ref = createParserForwardedToRef<aexpr>()
    let A4, a4ref = createParserForwardedToRef<aexpr>()
    let S1, s1ref = createParserForwardedToRef<stmnt>()
    let S2,s2ref = createParserForwardedToRef<stmnt>()

    
    
    let paexpr = A1
    let pbexpr = B1
    let pstmnt = S1




    let Qparse = pbexpr .>*> pchar '?' .>*>. paexpr .>*> pchar ':' .>*>. paexpr |>> (fun ((b,x),x2) -> Cond(b,x,x2))  (*Cond(fst (fst x), snd x, snd x))*)


    do a1ref := choice [Qparse;A2]

    let conjunctionParse = binop ( pstring "/\\") B2 pbexpr |>> Conj
    let disjunctionParse = binop (pstring "\\/") B2 pbexpr |>> (fun x -> fst x .||. snd x)
    
    do b1ref := choice [disjunctionParse;conjunctionParse;B2]

    let TParse = ptrue |>> (fun _ -> TT)
    let FParse = pfalse |>> (fun _ -> Not TT)
    let NotParse = unop (pchar '~') B2 |>> Not <?> "Not" 
    let EqParse = binop (pchar '=') A2 paexpr |>> Eq <?> "Equality"
    let InEqParse = binop (pstring "<>") A2 paexpr |>> (fun x -> fst x .<>. snd x)
    let LTParse = binop (pchar '<') A2 paexpr |>> Lt
    let LTEParse = binop (pstring "<=") A2 paexpr |>> (fun x -> (fst x .<=. snd x))
    let GTParse = binop (pchar '>') A2 paexpr |>> (fun x -> (fst x .>. snd x))
    let GTEParse = binop (pstring ">=") A2 paexpr |>> (fun x -> (fst x .>=. snd x))
    let ParBParse = parenthesise pbexpr


    do b2ref := choice [TParse;FParse;NotParse;EqParse;InEqParse;LTParse; LTEParse;GTParse;GTEParse;ParBParse]

     
    let AddParse = binop (pchar '+') A3 A2 |>> Add <?> "Add"
    let SubParse = binop (pchar '-') A3 A2 |>> (fun x -> Add(fst x, Mul(snd x, Num -1)))
    do a2ref := choice [AddParse; SubParse; A3]


    let MulParse = binop (pchar '*') A4 A3 |>> Mul <?> "Mul"

    let DivParse = binop(pchar '/') A4 A3 |>> Div <?> "Div" 

    let ModParse = binop(pchar '%') A4 A3 |>> Mod <?> "Mod" 
    do a3ref := choice[ MulParse; DivParse; ModParse; A4]


    let NParse = pint32 |>> Num <?> "Int"
    let VParse = pid |>> Var  <?> "String"
    let NegParse = unop (pchar '-') A4 |>> (fun x ->(Mul(Num -1, x)))
    let ParParse = parenthesise A1
    let MemReadParse = squares A1 |>> MemRead 
    let ReadParse = pread |>> (fun _ -> Read) 
    let RandomParse = prandom |>> (fun _ -> Random)
    do a4ref := choice  [ParParse;NegParse; NParse;  MemReadParse; ReadParse; RandomParse;VParse;]

    let SeqParse = binop(pchar ';') S2 pstmnt |>> Seq 

    do s1ref:= choice[SeqParse;S2] 

    let BracketsParse = brackets S1
    let AssignParse = binop (pstring ":=") pid A1 |>> Assign
    let DecParse = pdeclare >>. spaces1 >>. pid |>> Declare
    let IfParse = pif .>> spaces1 >>. ParBParse .>> spaces1 .>>. BracketsParse .>> spaces1 .>> pelse .>> spaces1 .>>. BracketsParse |>> (fun ((b,st),st1) -> If(b,st,st1))
    let ItParse = pif .>> spaces1 >>. ParBParse .>> spaces1 .>>. BracketsParse |>> IT  
    let MemWriteParse = binop (pstring ":=") (squares A1) A1 |>> MemWrite
    let WhileParse = pwhile >>. spaces1 >>. ParBParse .>> spaces1 .>> pdo .>>. BracketsParse |>> While 
    let allocParse = palloc >*>. parenthesise (binop (pchar ',') pid A1) |>> Alloc 
    let freeParse = pfree >*>. parenthesise (binop (pchar ',') A1 A1) |>> Free  
    do s2ref:= choice [BracketsParse;IfParse;ItParse;WhileParse;MemWriteParse;allocParse;freeParse;AssignParse;DecParse]

    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
