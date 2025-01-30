module FSharpKattis.Reduplication

let rec Reduplicate s num =
        match num with
        | 0 -> ()
        | _ -> printf "%s" s
               Reduplicate s (num - 1)


let solve (scanner : Kattio.Scanner) =
    let inputstr = scanner.Next()
    let inputnum = scanner.NextInt()

    Reduplicate inputstr inputnum


     
        
