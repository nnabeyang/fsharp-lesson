// For more information see https://aka.ms/fsharp-console-apps
open System

// 課題6-2: [<EntryPoint>]を使う方法
[<EntryPoint>]
let main args =
  match Array.contains "-hello" args with
    | true -> printfn "Hello World"
    | false -> printfn "I don't know"
  0
