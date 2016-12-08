open Fake

let private log label a = 
  a |> sprintf "%s: %A" label |> trace
  a

type private Dependency = 
  | Source of string
  | Dll of string

let private getSourceFile (identifier : string) (importFileDeclaration : string) = 
  importFileDeclaration.Substring(identifier.Length).Trim([|' ';'\"'; '@'|])

let private getAbsoluteFilePath sourceDirectory filePath = 
  try
  sourceDirectory </> filePath
  with
  | _ -> failwithf "Unable to concatentate paths \n%s\n%s" sourceDirectory filePath
  |> fileInfo
  |> fun fi -> fi.FullName

let private (|IsDependency|_|) (sourceDirectory, interactiveImport : string) = 

  match interactiveImport.StartsWith("#load") with
  | true -> 
    match interactiveImport.Contains("include.") with
    | true -> None
    | false -> interactiveImport |> getSourceFile "#load" |> getAbsoluteFilePath sourceDirectory |> Source |> Some
  | false -> 
    match interactiveImport.StartsWith("#r") with
    | true -> 
      match interactiveImport.Contains("/") with
      | true -> interactiveImport |> getSourceFile "#r" |> getAbsoluteFilePath sourceDirectory |> Dll |> Some
      | false -> interactiveImport |> getSourceFile "#r" |> Dll |> Some 
    | false -> 
      failwithf "Unrecognised Interactive import: %s" interactiveImport

let  (|IsIncludes|_|) (sourceDirectory, interactiveImport : string) = 
  match (interactiveImport.StartsWith("#load"), interactiveImport.Contains("include.")) with
  | (true, true) -> interactiveImport |> getSourceFile "#load" |> getAbsoluteFilePath sourceDirectory |> fileInfo |> Some
  | _ -> None 

let private getInteractiveImports (isIncludes: bool) (scriptPath : string) = 
  let lazyLines = 
    System.IO.File.ReadLines(scriptPath)
    |> Seq.filter(System.String.IsNullOrWhiteSpace >> not)

  match lazyLines |> Seq.tryHead, isIncludes with
  | None, true -> Seq.empty
  | Some h, false when h |> startsWith "#if"  |> not -> Seq.empty
  | _, true -> 
    lazyLines
    |> Seq.filter(startsWith "#")
  | _, false -> 
    lazyLines
    |> Seq.skip 1
    |> Seq.takeWhile(startsWith "#endif" >> not)
    |> Seq.toList
    |> List.toSeq

type private  ImportLine = 
  | Script of string
  | Source of string
  | Includes of string
  | Dll of string

let private isScriptOrSource l = 
  match l with
  |Script _
  |Source _ -> true
  |_ -> false

let private getImportLine (path: string) = 
  match( path.EndsWith(".fsx"), path.Contains("include"), path.EndsWith(".fs")) with
  | (true, true, _) -> Includes path
  | (true, false, _) -> Script path
  | (false, _, true) -> Source path
  | _ when path.EndsWith(".dll") -> Dll path
  | _ -> failwithf "Unexpected Path: %s" path

let private getImports node = 
  match node with
  | Script path -> getInteractiveImports false path
  | Source path -> Seq.empty
  | Includes path -> getInteractiveImports true path
  | _ -> failwithf "Unexpected node: %A" node

let private importPath node = 
  match node with
  | Script path
  | Source path
  | Dll path
  | Includes path -> path

let rec private removeLaterDuplicates l = 
  match l with
  | [] -> l
  | x::xs -> x:: removeLaterDuplicates (xs |> List.filter(fun i -> i <> x))

let rec private getTree (sources, references, processedPaths, unprocessedPaths : string list) = 
  match unprocessedPaths with
  | [] -> (sources, references)
  | x::xs -> 
    let node = x |> getImportLine
    match processedPaths |> Set.contains node with
    | true -> 
      getTree (sources, references, processedPaths, xs)
    | false -> 
      let nodePath = node |> importPath
      let nodeDir = nodePath |> fileInfo |> fun i -> i.Directory.FullName
      let imports = node |> getImports

      let notProcessed i = processedPaths |> Set.map(importPath) |> Set.forall((<>) i)

      // Get references .fx or .fsx source files 
      let sourceImports = 
        imports
        |> Seq.map(fun i -> 
          match nodeDir, i with
          | IsDependency s -> 
            match s with
            | Dependency.Source s -> Some s
            | Dependency.Dll _ -> None
          | _ -> None)
        |> Seq.filter(Option.isSome)
        |> Seq.map(Option.get)
        |> Seq.filter(notProcessed)
        |> List.ofSeq

      // Get references include.fsx files
      let includeImports = 
        imports
        |> Seq.map(fun i -> 
          match nodeDir, i with
          | IsIncludes s -> Some s.FullName
          | _ -> None)
        |> Seq.filter(Option.isSome)
        |> Seq.map(Option.get)
        |> Seq.filter(notProcessed)
        |> List.ofSeq

      let referenceImports = 
        imports
        |> Seq.map(fun i -> 
          match nodeDir, i with
          | IsDependency s -> 
            match s with
            | Dependency.Dll d -> Some d
            | Dependency.Source _ -> None
          | _ -> None)
        |> Seq.filter(Option.isSome)
        |> Seq.map(Option.get)

      let newReferences = 
        referenceImports
        |> Seq.fold(fun r i -> r |> Set.add i) references

      // Add current item to sources if all imports have been resolved
      let newSources = 
        match sourceImports, includeImports with
        | [], [] -> 
//          trace (sprintf "All dependencies resolved for %s" nodePath)
          let addedSource = 
            match node |> isScriptOrSource with
            | true -> [nodePath]
            | false -> []
          (sources @ addedSource, newReferences, processedPaths |> Set.add node, xs |> List.filter(notProcessed))
        | _ -> 
//          trace (sprintf "Outstanding dependencies for %s" nodePath)
          (sources, newReferences, processedPaths, (includeImports @ sourceImports @ [nodePath] @ xs ) |> List.filter(notProcessed))
      newSources  |> getTree


let private buildInner output (sources, references)  = 
  // traceImportant "References"
  // references |> Seq.iter(trace)
  let r = references |> Seq.map(fun r -> "-r " + r) |> String.concat " "
  let x = sources |> String.concat " "
  let exitCode = 
    ExecProcess(fun info -> 
      info.FileName <- "fsc.exe" 
      info.Arguments <- sprintf "--target:exe --noframework %s %s --out:%s" x r output
      ) (System.TimeSpan.FromSeconds(120.0))
  if exitCode = 0 then ()
  else failwithf "Build failed with exit code %i" exitCode

let ifExists f = 
    f |> fileInfo |> fun x -> x.Exists

let build outDir (outFile : string) scriptSrc = 
  let scriptFileInfo = scriptSrc |> fileInfo
  let fn = scriptSrc |> fileInfo |> fun fi -> fi.FullName
  let (sources, references) = getTree ([], Set.empty, Set.empty, [fn])
  (sources, references |> Seq.toList) |> buildInner outFile |> ignore
  CleanDir outDir
  MoveFile outDir outFile
  references |> Seq.filter ifExists |> Seq.iter(CopyFile outDir )
  let configFiles = 
    scriptFileInfo.Directory |> filesInDirMatching "*.config"
    |> Seq.iter(fun f -> CopyFile (outDir </> f.Name) f.FullName)
  ()

