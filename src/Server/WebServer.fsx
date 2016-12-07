#if INTERACTIVE

#load "../../lib/include.suave.swagger.fsx"
#load "../../paket-files/include-scripts/net46/include.nlog.fsx"
#load "../../paket-files/include-scripts/net46/include.newtonsoft.json.fsx"
#load "../../paket-files/include-scripts/net46/include.suave.fsx"
#load "../../paket-files/lawrencetaylor/FSharpHelpers/FSharp.CoreEx/Async.fs"
#load "../../paket-files/lawrencetaylor/FSharpHelpers/FSharp.CoreEx/Result.fs"
#load "../../paket-files/lawrencetaylor/FSharpHelpers/FSharp.CoreEx/Json.fs"
#load "../../paket-files/lawrencetaylor/FSharpHelpers/FSharp.CoreEx/Suave.fs"

#endif

open NLog
open Newtonsoft.Json
open FSharp.Core
open Suave
open Suave.Swagger

module Logging = 

  let private logger = NLog.LogManager.GetLogger("WebServer")

  let error (message : string) = 
    logger.Error(message)

  let logTimings (name : string) (f : 'a -> Async<'b>) : 'a -> Async<'b> =  
    let logResultF x = 
      let sw = System.Diagnostics.Stopwatch.StartNew()
      (
          f >> Async.map(fun y -> 
          sw.Stop()
          let elapsed = sw.ElapsedMilliseconds 
          logger.Trace( sprintf "Time to execute %s was %A%A%A \n %A" name elapsed System.Environment.NewLine x y) 
          y)) x
    logResultF



type ApiErrors = 
  | NotFound of string
  | UnhandledError of string

module Api = 

  open Swagger
  open Suave.Successful
  open FunnyDsl

  let mySettings = 
    Json.defaultSettings
    |> Json.withConverter (Converters.OptionConverter())
  let serialize<'a>  = Json.serialize mySettings
  let getParams<'a> req = HttpRequest.getBody<'a> mySettings req
  let fromFunction f = HttpRequest.makeRequestAdvanced mySettings getParams f

  let onResult r = 
    match r with
    | Result.Ok s -> s :> obj |> serialize |> OK
    | Result.Error e -> 
      match e with
      | ApiErrors.NotFound message -> Suave.RequestErrors.NOT_FOUND message
      | ApiErrors.UnhandledError message -> 
        Logging.error message |> ignore
        Suave.ServerErrors.INTERNAL_ERROR "Internal Error"

  let document tagName (routeString) (routeDescription :string) (f : 'a -> Async<Result.T<'b,_>>) = 
    let wrappedF = fromFunction (Logging.logTimings routeString f) onResult

    swagger {
      for route in posting <| simpleUrl routeString |> thenReturns wrappedF do
        yield description Of route is routeDescription
        yield route |> addResponse 200 "result" (Some typeof<'b>)
        yield parameter "request" Of route (fun p -> { p with Type = (Some typeof<'a>); In=Body })
        yield route |> tag tagName
    }

  let redirectToDocs =
    swagger {
      
      for route in getting <| simpleUrl "/" |> thenReturns (Redirection.redirect "/swagger/v2/ui/index.html") do
        yield description Of route is "Redirect to docs from home"
        yield route |> tag "Redirects"
    } 

module Calculator = 

  module Add =  

    [<SwaggerType(Name = "AddRequest")>] type Request = { X : int; Y : int}
    [<SwaggerType(Name = "AddResponse")>] type Response = { Sum : int}
    let execute {X = x; Y = y} = { Sum = x + y} |> Result.Ok |> Async.ret

let api () = 

  Swagger.swagger {

    yield Api.document "Calculator" "/calculate/add"  "Adds two numbers" (Calculator.Add.execute)
    yield Api.redirectToDocs 
  }






