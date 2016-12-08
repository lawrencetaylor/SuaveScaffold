module Result

  type T<'a, 'b> = 
    Ok of 'a
    | Error of 'b

  let ofOption error option = 
    match option with
    | None -> Error error
    | Some o -> Ok o

  let throwIfError result = 
    match result with
    | Ok a -> a
    | Error error -> failwithf "Result.Error: %A" error

  let ret a = Ok a

  let tryGetOk result = 
    match result with
    | Ok ok -> Some ok
    | Error _ -> None

  let ok (r : T<'a, _>) = (tryGetOk >> Option.get) r

  let apply (resultF : T<'a ->'b, _>) (resultA : T<'a, _>) = 
    match resultF, resultA with
    | Ok f, Ok a -> Ok (f a)
    | Error fError, _ -> Error fError
    | _ , Error aError -> Error aError

  let map (fOk, fError) result = 
    match result with
    | Ok a  -> Ok (fOk a)
    | Error error -> Error(fError error)

  let mapOk f = map (f, id)
  let mapError f = map (id, f)

  let bindOk (f : 'a -> T<'b, 'c>) aR = 
    match aR with
    | Ok a -> a |> f
    | Error x -> Error x

  let ofAsyncToAsync xRA = 
    match xRA with
    | Ok xA -> 
      async { 
        let! x = xA 
        return Ok x}
    | Error e -> async { return Error e}

  let traverse sa = 

    let rec innerTraverse sOfR current = 
      match sOfR |> Seq.tryHead with
      | None -> Ok current
      | Some (Ok x) -> innerTraverse (Seq.tail sOfR) (current |> Seq.append [x])
      | Some (Error e) -> Error e

    innerTraverse sa Seq.empty