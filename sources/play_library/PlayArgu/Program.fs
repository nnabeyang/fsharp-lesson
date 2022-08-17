// For more information see https://aka.ms/fsharp-console-apps
open System

// 課題6-1: Environment.GetCommandLineArgs()を使う方法
let args = Environment.GetCommandLineArgs()
match Array.contains "-hello" args with
  | true -> printfn "Hello World"
  | false -> printfn "I don't know"

