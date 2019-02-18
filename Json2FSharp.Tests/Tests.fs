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
let ``Should and The in begin of rootObject`` () =
    let input = "{}"

    let result = generateRecords FsharpCommon.fixName "3" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = name; Fields = []}] when name = "The3" -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse string`` () =
    let root = "Root"
    let input = @"{ ""name"": ""test"" }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Name"; Type = "string"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse int64`` () =
    let root = "Root"
    let input = @"{ ""age"": 4 }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse float`` () =
    let root = "Root"
    let input = @"{ ""age"": 4.2 }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Age"; Type = "float"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse object`` () =
    let root = "Root"
    let input = @"{ ""someObj"": {} }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [ { Name = "SomeObj"; Fields = [] }
           { Name = name; Fields = [ { TypeId = _; Name = "SomeObj"; Type = "SomeObj"; Template = "%s" }] }
         ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse null`` () =
    let root = "Root"
    let input = @"{ ""emptyObj"": null }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "EmptyObj"; Type = "Object"; Template = "%s option" }] } ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse DateTimeOffset`` () =
    let root = "Root"
    let input = @"{ ""date"": ""2012-04-23T18:25:43.511Z"" }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Date"; Type = "DateTimeOffset"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse bool`` () =
    let root = "Root"
    let input = @"{ ""isOk"": true }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "IsOk"; Type = "bool"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array`` () =
    let root = "Root"
    let input = @"{ ""arr"": [] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64`` () =
    let root = "Root"
    let input = @"{ ""arr"": [1, 2] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "int64"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()


[<Fact>]
let ``Should parse array with float`` () =
    let root = "Root"
    let input = @"{ ""arr"": [1.3, 2.2] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with string`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""a"", ""b""] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "string"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with bool`` () =
    let root = "Root"
    let input = @"{ ""arr"": [true, false] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "bool"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with dateTimes`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""2012-04-23T18:25:43.511Z"", ""2012-04-23T16:25:43.511Z""] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "DateTimeOffset"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with objects`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{}, {}] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = []; }
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with arrays`` () =
    let root = "Root"
    let input = @"{ ""arr"": [[], []] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()


[<Fact>]
let ``Should parse array with float and int64s`` () =
    let root = "Root"
    let input = @"{ ""arr"": [3, 2.3] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64s and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [3, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "int64"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with floats and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [3.2, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64s and floats and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [1, 3.2, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with strings and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""Test"", null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "string"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with dateTimes and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""2012-04-23T18:25:43.511Z"", null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "DateTimeOffset"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with bools and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [true, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "bool"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with objects and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{}, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [] }
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with array and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [[], null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64s and strings`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""test"", 43] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()


[<Theory()>]
[<InlineData("\"test\"", "3")>]
[<InlineData("\"test\"", "3.4")>]
[<InlineData("\"test\"", "true")>]
[<InlineData("\"test\"", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("\"test\"", "{}")>]
[<InlineData("\"test\"", "[]")>]
[<InlineData("2", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("2", "true")>]
[<InlineData("2", "{}")>]
[<InlineData("2", "[]")>]
[<InlineData("2.1", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("2.1", "true")>]
[<InlineData("2.1", "{}")>]
[<InlineData("2.1", "[]")>]
[<InlineData("true", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("true", "{}")>]
[<InlineData("true", "[]")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "{}")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "[]")>]
let ``Should parse array with floats and strings`` firstValue secondValue =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [%s, %s] }" firstValue secondValue

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Theory()>]
[<InlineData("\"test\"", "3")>]
[<InlineData("\"test\"", "3.4")>]
[<InlineData("\"test\"", "true")>]
[<InlineData("\"test\"", "\"2012-04-23T18:25:43.511Z\"")>]
[<InlineData("\"test\"", "{}")>]
[<InlineData("\"test\"", "[]")>]
[<InlineData("2", "\"2012-04-23T18:25:43.511Z\"")>]
[<InlineData("2", "true")>]
[<InlineData("2", "{}")>]
[<InlineData("2", "[]")>]
[<InlineData("2.1", "\"2012-04-23T18:25:43.511Z\"")>]
[<InlineData("2.1", "true")>]
[<InlineData("2.1", "{}")>]
[<InlineData("2.1", "[]")>]
[<InlineData("true", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("true", "{}")>]
[<InlineData("true", "[]")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "{}")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "[]")>]
let ``Should parse array with floats and strings and null`` firstValue secondValue =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [%s, %s, null] }" firstValue secondValue

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Theory()>]
[<InlineData("\"test\"", "string")>]
[<InlineData("2", "int64")>]
[<InlineData("2.1", "float")>]
[<InlineData("true", "bool")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "DateTimeOffset")>]
let ``Should parse array and merge two objects with same type field`` value resType =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [{ ""age"": %s }, { ""age"": %s }] }" value value

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = arrName; Fields = [ { TypeId = _; Name = "Age"; Type = fieldType; Template = "%s" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }          
         ] when name = root && arrName = "Arr" && fieldType = resType ->         
        pass ()
    | _ -> fail ()

[<Theory()>]
[<InlineData("\"test\"", "string")>]
[<InlineData("2", "int64")>]
[<InlineData("2.1", "float")>]
[<InlineData("true", "bool")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "DateTimeOffset")>]
let ``Should parse array and merge two objects with same type field with null`` value resType =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [{ ""age"": %s }, { ""age"": %s }, {""age"": null }] }" value value

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [ { TypeId = _; Name = "Age"; Type = fieldType; Template = "%s option" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }
         ] when name = root && fieldType = resType ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and merge two objects with different fields`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{ ""age"": 4 }, { ""name"": ""Name"" }] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s option" };
                                       { TypeId = _; Name = "Name"; Type = "string"; Template = "%s option" }
                                     ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }
         ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and merge two lists`` () =
    let root = "Root"
    let input = @"{ ""arr"": [[{ ""age"": 4 }], [{ ""name"": ""Name"" }]] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s option" };
                                       { TypeId = _; Name = "Name"; Type = "string"; Template = "%s option" }
                                     ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list list" }] }
         ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and ignore nulls`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{ ""age"": 2 }, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = arrName; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s option list" }] }
         ] when name = root && arrName = "Arr" ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and merge two objects with int64s and floats`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{ ""age"": 2 }, { ""age"": 4.2 }] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = arrName; Fields = [ { TypeId = _; Name = "Age"; Type = "float"; Template = "%s" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }
         ] when name = root && arrName = "Arr" ->         
        pass ()
    | _ -> fail ()


[<Fact>]
let ``Should remove incorrect symbols`` () =
    let rootName = "root!@#$^&*()+=\\.,~`�;%:?*)! "
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName rootName FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "Root"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should left numbers`` () =
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName "root1" FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "Root1"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fix underscore and minus`` () =
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName "root-test" FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "RootTest"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fix add The before`` () =
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName "1root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "The1root"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should rename objects if with same names`` () =
    let input = @"{ ""obj"": { ""field"": 2 }, ""next"": { ""obj"": { ""age"": 2 } } }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Obj0"; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s" }] }
           { Name = "Next"; Fields = [ { TypeId = _; Name = "Obj"; Type = "Obj0"; Template = "%s" } ]}
           { Name = "Obj1"; Fields = [ { TypeId = _; Name = "Field"; Type = "int64"; Template = "%s" } ]}
           { Name = "Root"; Fields = [ { TypeId = _; Name = "Obj"; Type = "Obj1"; Template = "%s" }
                                       { TypeId = _; Name = "Next"; Type = "Next"; Template = "%s" }
                                     ] }
         ] -> pass ()
    | _ -> fail ()