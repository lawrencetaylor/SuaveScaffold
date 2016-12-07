#if INTERACTIVE

#load "../../paket-files/include-scripts/net46/include.suave.fsx"
#load "./WebServer.fsx"

#endif

open Suave
open System.Configuration

module IISHelpers =
  open System 

  /// Port specified by IIS HttpPlatformHandler
  let httpPlatformPort =
      match Environment.GetEnvironmentVariable("HTTP_PLATFORM_PORT") with
      | null -> None
      | value ->
          match Int32.TryParse(value) with
          | true, value -> Some value
          | false, _ -> None

  open Suave
  open Suave.Filters
  open Suave.Operators
  open Suave.Successful

[<EntryPoint>]
let main argv =
  let config = defaultConfig
  let config =
      match IISHelpers.httpPlatformPort with
      | Some port ->
          { config with
              bindings = 
                [ { scheme = Protocol.HTTP
                    socketBinding = 
                      {
                        port = 8083 |> System.Convert.ToUInt16
                        ip = System.Net.IPAddress.Any } 
                  }
                ]
          }
      | None -> 
        { config with bindings = [ HttpBinding.mkSimple Protocol.HTTP "127.0.0.1" 8083 ] }
  let c = 
    { config with 
        errorHandler = 
          fun e v ct -> 
            let msg : string = sprintf "Error: %A" e
            printfn "ERROR: %s" msg
            WebServer.Logging.error msg |> ignore
            async { return Some ct}}
  startWebServer c (WebServer.api ()).App
  0 // return an integer exit code