module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    

    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc    : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "not implemented"
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

    let charConverter chars = List.fold(fun acc x -> acc + string x) "" chars

    let parseString= let charList = many (satisfy (fun x -> System.Char.IsAscii x))
                     let str = List.map (fun x -> charConverter x)charList
                     str |>> (fun x -> x.Replace ('"',' '))
                     
    

    let pid = (pletter <|> pchar '_') .>>. (many (palphanumeric <|> pchar '_')) |>> fun(x,y) ->  string x + charConverter y
    
    let unop _ = failwith "not implemented"
    let binop _ = failwith "not implemented"

    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()

    //let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    //do tref := choice [AddParse; ProdParse]

    //let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    //do pref := choice [MulParse; AtomParse]

    //let NParse   = pint32 |>> Num <?> "Int"
    //let ParParse = parenthesise TermParse
    //do aref := choice [NParse; ParParse]

    let paexpr = pstring "not implemented" |>> (fun _ -> Num 42)

    let pbexpr = pstring "not implemented" |>> (fun _ -> TT)

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
