module FSharpKattis.LeggjaSaman

let solve (scanner : Kattio.Scanner) =
    let a = scanner.NextInt()
    let b = scanner.NextInt()
    
    printfn "%d" (a + b)