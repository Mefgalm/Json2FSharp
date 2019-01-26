[<RequireQualifiedAccess>]
module FsharpNewtonsoftHandler


//let joinByNewLine = sprintf "%s\n%s"

//let typeIdentificationToString (typeIdentification: TypeInfo) =
//    match typeIdentification.Attributes with
//    | [] -> sprintf "type %s =" typeIdentification.Name
//    | attribues -> 
//        attribues
//             |> List.fold joinByNewLine ""
//             |> (fun x -> sprintf "\t%s\ntype %s =" x typeIdentification.Name)

//let fieldIdentificationToString (fieldIdentification: FieldInfo) =
//   match fieldIdentification.Attributes with
//   | [] -> sprintf "%s: %s" fieldIdentification.Name fieldIdentification.Type
//   | attributes -> sprintf "\n%s\n%s: %s" (attributes |> List.reduce joinByNewLine) fieldIdentification.Name fieldIdentification.Type
    
//let fieldsToString =
//    function
//        | [] -> ""
//        | fields ->
//                fields
//                |> List.map fieldIdentificationToString
//                |> List.reduce (sprintf "%s\n\t  %s")

//let typeDefinitionToString typeDefinition =
//    sprintf "%s\n\t{ %s }" (typeIdentificationToString typeDefinition.TypeIdentification) (fieldsToString typeDefinition.FieldIdentifications)

//let allTypeDefinitionsToString = List.map typeDefinitionToString >> List.reduce (sprintf "%s\n\n%s")

//let opensToString<'a> = Seq.fold (sprintf "%s\n%s") ""
    
//let newtonsoftToString file = 
//    sprintf "module %s\n%s\n\n%s" file.ModuleName (opensToString file.Opens) (allTypeDefinitionsToString file.TypeDefinitions)
    


open Types

let private fixName = FsharpCommon.fixName

type Field = 
    { Name: string
      Type: string
      Attributes: string list }

type Type = 
    { Name: string
      Fields: Field list }

let rec fieldHandler collectionGenerator name = 
    let getName { Name = name; Type = _; Attributes = _ } = name
    let generateAttributes name = [sprintf "[<JsonProperty(\"%s\")>]" name]
    
    function
    | JBool ->                  { Name = name |> fixName; Type = "bool"; Attributes = (generateAttributes name) }
    | JBoolOption ->            { Name = name |> fixName; Type = "bool option"; Attributes = (generateAttributes name) }
    | JNull ->                  { Name = name |> fixName; Type = "Object option"; Attributes = (generateAttributes name) } 
    | JInt ->                   { Name = name |> fixName; Type = "int64"; Attributes = (generateAttributes name) }
    | JIntOption ->             { Name = name |> fixName; Type = "int64 option"; Attributes = (generateAttributes name) }
    | JFloat ->                 { Name = name |> fixName; Type = "float"; Attributes = (generateAttributes name) } 
    | JFloatOption ->           { Name = name |> fixName; Type = "float option"; Attributes = (generateAttributes name) }
    | JString ->                { Name = name |> fixName; Type = "string"; Attributes = (generateAttributes name) }
    | JDateTimeOffset ->        { Name = name |> fixName; Type = "DateTimeOffset"; Attributes = (generateAttributes name) }
    | JDateTimeOffsetOption ->  { Name = name |> fixName; Type = "DateTimeOffset option"; Attributes = (generateAttributes name) }
    | JStringOption ->          { Name = name |> fixName; Type = "string option"; Attributes = (generateAttributes name) }
    | JEmptyObjectOption ->     { Name = name |> fixName; Type = "Object option"; Attributes = (generateAttributes name) }
    | JObject _ ->              { Name = name |> fixName; Type = name; Attributes = (generateAttributes name) }
    | JObjectOption _ ->        { Name = name |> fixName; Type = sprintf "%s %s" name "option"; Attributes = (generateAttributes name) }
    | JArray obj ->             { Name = name |> fixName; Type = fieldHandler collectionGenerator name obj |> getName |> collectionGenerator; Attributes = (generateAttributes name) }
    | JArrayOption obj ->       { Name = name |> fixName; Type = fieldHandler collectionGenerator name obj |> getName |> collectionGenerator; Attributes = (generateAttributes name) }
    | _ -> failwith "translateToString unexcpected"

let typeHandler name fields = { Name = name |> fixName; Fields = fields }

let toView types = "newtonsoft"