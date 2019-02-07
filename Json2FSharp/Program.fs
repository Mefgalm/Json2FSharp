﻿open System
open FParsec
open Newtonsoft.Json
open Microsoft.FSharp
open Microsoft.FSharp.Reflection
open JsonParser
open Types


type OptionConverter() =
    inherit JsonConverter()
    
    override x.CanConvert(t) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override x.WriteJson(writer, value, serializer) =
        let value = 
            if value = null then null
            else 
                let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]  
        serializer.Serialize(writer, value)

    override x.ReadJson(reader, t, existingValue, serializer) =        
        let innerType = t.GetGenericArguments().[0]
        let innerType = 
            if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
            else innerType        
        let value = serializer.Deserialize(reader, innerType)
        let cases = FSharpType.GetUnionCases(t)
        if value = null then FSharpValue.MakeUnion(cases.[0], [||])
        else FSharpValue.MakeUnion(cases.[1], [|value|]) 

let generateRecords collectionGenerator (str: string) =
    match parseJsonString str with
    | Success(result, _, _) -> JsonResult.Ok ^ buildTypes collectionGenerator result        
    | Failure(errorMsg, _, _) -> JsonResult.Error ^ errorMsg


type Node = 
    { Id: int
      Name: string
      Reference: string
      RefId: int }

[<EntryPoint>]
let main argv =

    let testExample = @"
    {
        ""test1"": {
            ""Welcome"" : {}
        },
        ""test2"": {
            ""Welcome"" : { ""value"": 4 }
        }
    }"

    let output = (generateRecords FsharpCommon.listGenerator testExample) |> FsharpSimpleTypeHandler.toView

    printfn "%s" output


    Console.ReadKey() |> ignore
    0 // return an integer exit code
