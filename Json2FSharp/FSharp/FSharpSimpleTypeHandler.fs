[<RequireQualifiedAccess>]
module FsharpSimpleTypeHandler

open Types

type Field =
    { Name: string
      Type: string }

type Type =
    { Name: string
      Fields: Field list }

let private fixName = FsharpCommon.fixName

let toView types = types |> List.map(fun x -> x.ToString()) |> List.fold(+) ""

let typeHandler name fields = { Name = name |> fixName; Fields = fields }

let rec fieldHandler collectionGenerator name = 
    let getName { Name = name; Type = _ } = name

    function
    | JBool ->                  { Name = name |> fixName; Type = "bool" }
    | JBoolOption ->            { Name = name |> fixName; Type = "bool option" }
    | JNull ->                  { Name = name |> fixName; Type = "Object option" } 
    | JInt ->                   { Name = name |> fixName; Type = "int64" }
    | JIntOption ->             { Name = name |> fixName; Type = "int64 option" }
    | JFloat ->                 { Name = name |> fixName; Type = "float"} 
    | JFloatOption ->           { Name = name |> fixName; Type = "float option" }
    | JString ->                { Name = name |> fixName; Type = "string" }
    | JDateTimeOffset ->        { Name = name |> fixName; Type = "DateTimeOffset" }
    | JDateTimeOffsetOption ->  { Name = name |> fixName; Type = "DateTimeOffset option" }
    | JStringOption ->          { Name = name |> fixName; Type = "string option" }
    | JEmptyObjectOption ->     { Name = name |> fixName; Type = "Object option" }
    | JObject _ ->              { Name = name |> fixName; Type = name }
    | JObjectOption _ ->        { Name = name |> fixName; Type = sprintf "%s %s" name "option" }
    | JArray obj ->             { Name = name |> fixName; Type = fieldHandler collectionGenerator name obj |> getName |> collectionGenerator }
    | JArrayOption obj ->       { Name = name |> fixName; Type = fieldHandler collectionGenerator name obj |> getName |> collectionGenerator }
    | _ -> failwith "translateToString unexcpected"

