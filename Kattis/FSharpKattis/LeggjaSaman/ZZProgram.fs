// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

[<EntryPoint>]
let main argv =
    FSharpKattis.LeggjaSaman.solve(Kattio.Scanner())
    0