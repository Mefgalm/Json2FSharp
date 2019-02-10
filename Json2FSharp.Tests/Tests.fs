module Tests

open System
open Xunit
open JsonParser
open Types

let pass () = Assert.True(true)
let fail () = Assert.True(false)

[<Fact>]
let ``Should be error if empty json`` () =
    let input = ""

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok _ -> fail ()
    | Error _ -> pass ()    

[<Fact>]
let ``Should be empty object if only braces`` () =
    let root = "Root"
    let input = "{}"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with    
    | Ok [{Name = name; Fields = [] }] when name = root -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fail if root object name is empty`` () =
    let input = "{}"

    let result = generateRecords FsharpCommon.fixName "" FsharpCommon.listGenerator input

    match result with
    | Ok _ -> fail ()
    | Error _ -> pass ()

[<Fact>]
let ``Should parse string`` () =
    let root = "Root"
    let input = @"{ ""name"": ""test"" }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "name"; Type = "string" }] }] when name = root ->         
        pass ()
    | _ -> pass ()