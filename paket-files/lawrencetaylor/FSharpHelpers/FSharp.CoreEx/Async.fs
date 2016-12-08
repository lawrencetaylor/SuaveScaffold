module Async

  let bind f xAsync = 
    async { 
      let! x = xAsync
      let! y = f x
      return y
    }

  let ret a = async { return a }

  let map f xAsync = 
    async {
      let! x = xAsync
      return f x
    }

  let toAsyncSeq x = 
    let rec traverseSeq f list =
      // define the monadic functions
      let (>>=) x f = bind f x
      let retn = ret

      // define a "cons" function
      let cons head tail = [head] |> Seq.ofList |> Seq.append(tail)

      // right fold over the list
      let initState = retn Seq.empty
      let folder head tail = 
          f head >>= (fun h -> 
          tail >>= (fun t ->
          retn (cons h t) ))

      Seq.foldBack folder list initState 
    traverseSeq id x

  let inline startAsPlainTask (work : Async<unit>) = 
      System.Threading.Tasks.Task.Factory.StartNew(fun () -> work |> Async.RunSynchronously)

  let sleepForSeconds seconds = 
    Async.Sleep (seconds*1000)