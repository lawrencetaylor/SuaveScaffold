namespace Suave
  
  module HttpRequest =

    open Suave
    open Suave.Successful
    open Newtonsoft.Json

    let deserialize<'a> settings = Json.deserialize<'a> settings

    let serialize settings : (obj -> string) = Json.serialize settings

    open Microsoft.FSharp.Reflection
    let getBody<'a> settings request = 
      request.rawForm
      |> System.Text.Encoding.UTF8.GetString
      |> deserialize<'a> settings

    let tryGetHeader<'a> settings (request : HttpRequest) (headerKey : string) : 'a option = 
      request.headers
      |> List.tryFind(fst >> (=) (headerKey.ToLower()))
      |> Option.map(fun (_, json) -> deserialize<'a> settings json)

    let makeRequestAdvanced<'a, 'b, 'c> settings (p : HttpRequest -> 'a) (f : 'a -> Async<'b>) (onResult : 'b -> WebPart)  = 
      fun (ctx : HttpContext) -> 
        async {
          let req = p ctx.request
          let! res = f req
          return! onResult res ctx
        }

    let private makeRequest<'a, 'b> settings (f : 'a -> Async<'b>) (onResult : 'b -> WebPart)  = 
      makeRequestAdvanced settings (getBody<'a> settings) f onResult

    let private makeRequestSimple<'a, 'b> settings (f : 'a -> Async<'b>) : WebPart = makeRequest settings f (serialize settings >> OK)

    let fromFunction settings (f : 'a -> Async<'b>) (onResult : 'b -> WebPart) = makeRequest settings f onResult

    let fromFunctionSimple settings (f : 'a -> Async<'b>) = makeRequestSimple settings f

