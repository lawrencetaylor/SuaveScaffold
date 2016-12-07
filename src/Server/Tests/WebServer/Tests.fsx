#if INTERACTIVE

#load "../../../../paket-files/include-scripts/net46/include.fuchu.fsx"
#load "../../../../paket-files/include-scripts/net46/include.nlog.fsx"
#load "../../../../paket-files/include-scripts/net46/include.unquote.fsx"
#load "../../../../paket-files/lawrencetaylor/FSharpHelpers/FSharp.CoreEx/Json.fs"
#load "./Client.fsx"

#endif

open Fuchu
open Swensen.Unquote
open System.Collections.Generic
open NLog


let tests host= 

  let client = Client.create host

  [
    testCase "Can add two numbers" <| fun _ -> 
      test <@ client.add 1 2 = 3 @>
  ]

[<EntryPoint>] 
let main argv = 
  "http://localhost:8083"
  |> tests
  |> testList "Web Server Tests"
  (* You can filter on test name *)
  // |> Test.filter(fun c -> c.Contains("XYZ")) 
  |> runParallel

  

