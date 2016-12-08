namespace Newtonsoft.Json

open Newtonsoft.Json

module Json = 

  let defaultSettings = JsonSerializerSettings()

  let withConverter converter (settings : JsonSerializerSettings) = 
    settings.Converters.Add(converter)
    settings

  let serialize (settings : JsonSerializerSettings) (x : obj) = JsonConvert.SerializeObject(x, settings)
  
  let deserialize<'a> (settings : JsonSerializerSettings) json = JsonConvert.DeserializeObject<'a>(json, settings)

module Converters = 

  open FSharp.Reflection

  let private (|IsOptional|_|) (t : System.Type) = 
    match t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Option<_>> with
    | true -> Some t.GenericTypeArguments.[0]
    | false -> None

  let private (|IsSome|_|) v = 
    let (info, instance) = FSharpValue.GetUnionFields(v, v.GetType())
    match info.Name with
    | "Some" -> 
      let [|value|] = instance
      Some value
    | _ -> None
  let private makeOption value = 
    let valueType = value.GetType()
    let optionType = typedefof<Option<_>>.MakeGenericType(valueType)
    FSharpType.GetUnionCases(optionType) 
    |> Array.filter(fun u -> u.Name = "Some") 
    |> Array.exactlyOne
    |> fun c -> FSharpValue.MakeUnion(c, [|value|])

  let private getOption value = 
    let valueType = value.GetType()
    let a = FSharpValue.GetUnionFields(value, valueType)
    a

  type public OptionConverter() =
    inherit JsonConverter()

    override __.CanConvert(objectType) =
      objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<Option<_>>

    override __.WriteJson(writer, value, serializer) =
      match value with
      | IsSome o -> serializer.Serialize(writer, o)
      | _ -> ()
      
    override __.ReadJson(reader, destinationType, existingValue, serializer) =

        match reader.TokenType with
        | JsonToken.StartArray
        | JsonToken.StartObject -> 
          match destinationType with
          | IsOptional targetType -> serializer.Deserialize(reader, targetType) |> makeOption
          | _ -> serializer.Deserialize(reader, destinationType)
          
        | JsonToken.Date
        | JsonToken.Boolean
        | JsonToken.String
        | JsonToken.Integer -> 
          let cast s = s :> obj
          let getValue : System.Type -> obj -> obj = 
            function
            | t when t = typeof<System.Int32> -> System.Convert.ToInt32 >> cast
            | t when t = typeof<System.Int64> -> System.Convert.ToInt64 >> cast
            | t when t = typeof<System.DateTime> -> System.Convert.ToDateTime >> cast
            | t when t = typeof<System.Boolean> -> System.Convert.ToBoolean >> cast
            | t when t = typeof<System.String> -> System.Convert.ToString >> cast
            | _ -> failwithf "Un supported integer type: %A" destinationType
          match destinationType with
          | IsOptional targetType -> reader.Value |> getValue targetType |> makeOption
          | _ -> reader.Value |> getValue destinationType

        | JsonToken.Float -> 
          let value = 
            serializer.FloatParseHandling 
            |> function | FloatParseHandling.Double -> System.Convert.ToDouble reader.Value :> obj
                        | FloatParseHandling.Decimal -> System.Convert.ToDecimal reader.Value :> obj
          match destinationType with
          | IsOptional targetType -> value |> makeOption
          | _ -> value

        | JsonToken.Null -> 
          match destinationType with
          | IsOptional targetType -> None :> obj
          | _ -> null
          
        | t -> failwithf "Unsupported token %A" t
