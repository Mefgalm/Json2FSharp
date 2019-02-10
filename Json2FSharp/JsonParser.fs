﻿module JsonParser

open System
open FParsec
open Microsoft.FSharp
open Types

let ws = spaces 
let str s = pstring s

let stringLiteral =
    let escape = anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c

    let unicodeEscape =
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9 
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    between (str "\"") (str "\"")
            (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                          (str "\\" >>. (escape <|> unicodeEscape)))

let stringOrDateTime (str: string) =
    if DateTimeOffset.TryParse(str, ref (DateTimeOffset())) then JDateTimeOffset
    else JString

let jstringOrDate = stringLiteral |>> stringOrDateTime

let jnumber = pfloat |>> (fun x -> if x = Math.Floor(x) then JInt else JFloat)

let jtrue  = stringReturn "true"  JBool
let jfalse = stringReturn "false" JBool
let jnull  = stringReturn "null" JNull

let jvalue, jvalueRef = createParserForwardedToRef() 

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

let jlist   = listBetweenStrings "[" "]" jvalue JList
let jobject = listBetweenStrings "{" "}" keyValue JObject

do jvalueRef := choice [jobject
                        jlist
                        jstringOrDate
                        jnumber
                        jtrue
                        jnull
                        jfalse]

let json = ws >>. jvalue .>> ws .>> eof

let parseJsonString str = run json str

let inline (^) f x = f x
let (<||>) f1 f2 x = f1 x || f2 x
let (<&&>) f1 f2 x = f1 x && f2 x

let isDateTimeOffset = function JDateTimeOffset _ | JDateTimeOffsetOption _ -> true | _ -> false
let isArray = function JArray _ | JArrayOption _ -> true | _ -> false
let isList = function JList _ -> true | _ -> false
let isString = function JStringOption | JString -> true | _ -> false
let isNumber = function JInt | JFloat| JIntOption | JFloatOption -> true | _ -> false
let isNull = function JNull -> true | _ -> false
let isBool = function JBool | JBoolOption -> true | _ -> false
let isObject = function JObject _ | JObjectOption _ -> true | _ -> false

let isDateTimeOption = function JDateTimeOffsetOption -> true | _ -> false
let isBoolOption = function JBoolOption -> true | _ -> false
let isStringOption = function JStringOption -> true | _ -> false
let isArrayOption = function JArrayOption _ -> true | _ -> false
let isObjectOption = function JObjectOption _ -> true | _ -> false
let isNumberOption = function JIntOption | JFloatOption -> true | _ -> false
let typeOrder = 
    function 
    | JInt | JIntOption -> 1 
    | JFloat | JFloatOption -> 2
    | _ -> failwith "Not number type"

let checkStringOption = List.exists (isNull <||> isStringOption)
let checkArrayOption = List.exists (isNull <||> isArrayOption)
let checkObjectOption = List.exists (isNull <||> isObjectOption)
let checkNumberOption = List.exists (isNull <||> isNumberOption)
let checkBoolOption = List.exists (isNull <||> isBoolOption)
let checkDateTimeOption = List.exists (isNull <||> isDateTimeOption)

let (|EmptyList|_|) =
    function 
    | [] -> Some EmptyList
    | _ -> None

let (|NullList|_|) =
    function 
    | list when list |> List.forall isNull -> Some ^ NullList
    | _ -> None

let (|NumberList|_|) =
    function 
    | list when list |> List.forall (isNumber <||> isNull) -> Some ^ NumberList list
    | _ -> None

let (|StringList|_|) =
    function 
    | list when list |> List.forall (isString <||> isNull) -> Some ^ StringList list
    | _ -> None

let (|DateTimeOffsetList|_|) =
    function 
    | list when list |> List.forall (isDateTimeOffset <||> isNull) -> Some ^ DateTimeOffsetList list
    | _ -> None

let (|BoolList|_|) =
    function 
    | list when list |> List.forall (isBool <||> isNull) -> Some ^ BoolList list
    | _ -> None

let (|ObjectList|_|) =
    function 
    | list when list |> List.forall (isObject <||> isNull) -> Some ^ ObjectList list
    | _ -> None

let (|ListList|_|) =
    function 
    | list when list |> List.forall (isList <||> isNull) -> Some ^ ListList list
    | _ -> None

let (|ArrayList|_|) =
    function 
    | list when list |> List.forall (isArray <||> isNull) -> Some ^ ArrayList list
    | _ -> None

let rec aggreagateListToSingleType jsonList =
    let getOptionType isOption istanceType =
        match istanceType, isOption with
        | JInt, true -> JIntOption
        | JFloat, true -> JFloatOption
        | JBool, true -> JBoolOption
        | JString, true -> JStringOption
        | JDateTimeOffset, true -> JDateTimeOffsetOption
        | JObject x, true -> JObjectOption x
        | JArray x, true -> JArrayOption x
        | x, _ -> x

    match jsonList with
    | EmptyList -> JEmptyObject
    | NullList -> JEmptyObjectOption
    | StringList list -> JString |> getOptionType (list |> checkStringOption)
    | DateTimeOffsetList list -> JDateTimeOffset |> getOptionType (list |> checkDateTimeOption)
    | BoolList list -> JBool |> getOptionType (list |> checkBoolOption)
    | NumberList list ->
            list 
            |> List.filter (not << isNull)
            |> List.distinct
            |> List.map(fun x -> (x, typeOrder x))
            |> List.maxBy snd
            |> fst
            |> getOptionType (list |> checkNumberOption)
    | ObjectList list ->
            list 
            |> List.filter (not << isNull) 
            |> List.map(function JObject list | JObjectOption list -> list)
            |> List.collect id
            |> List.groupBy fst
            |> List.map(fun (key, value) -> (key, (aggreagateListToSingleType (value |> List.map snd))))
            |> JObject
            |> getOptionType (list |> checkObjectOption)
    | ListList list ->
            list 
            |> List.filter (not << isNull)
            |> List.map(function JList x -> x)
            |> List.collect id           
            |> aggreagateListToSingleType
            |> JArray
            |> getOptionType (list |> checkArrayOption)    
    | ArrayList list ->
            list 
            |> List.filter (not << isNull) 
            |> List.map(function JArray list | JArrayOption list -> list)
            |> aggreagateListToSingleType
            |> JArray
            |> getOptionType (list |> checkArrayOption) 
    | _ -> JEmptyObjectOption

let private castArray json = 
    let rec recCastArray json =
         json 
         |> List.map(fun (key, value) ->
                     match value with
                     | JObject list -> key, JObject ^ recCastArray list
                     | JList list -> key, JArray ^ aggreagateListToSingleType list
                     | _ -> key, value)   
    
    recCastArray json |> List.head

let rec private extractObject json =
    match json with
    | JArray obj 
    | JArrayOption obj -> extractObject obj
    | JObject _ 
    | JObjectOption _  -> Some json
    | _ -> None

let rec private fieldHandler fixName idGenerator collectionGenerator name = 
    let getName { Name = name; Type = _ } = name    

    function
    | JBool ->                  { TypeId = None; Name = name |> fixName; Type = "bool" }
    | JBoolOption ->            { TypeId = None; Name = name |> fixName; Type = "bool option" }
    | JNull ->                  { TypeId = None; Name = name |> fixName; Type = "Object option" } 
    | JInt ->                   { TypeId = None; Name = name |> fixName; Type = "int64" }
    | JIntOption ->             { TypeId = None; Name = name |> fixName; Type = "int64 option" }
    | JFloat ->                 { TypeId = None; Name = name |> fixName; Type = "float"} 
    | JFloatOption ->           { TypeId = None; Name = name |> fixName; Type = "float option" }
    | JString ->                { TypeId = None; Name = name |> fixName; Type = "string" }
    | JDateTimeOffset ->        { TypeId = None; Name = name |> fixName; Type = "DateTimeOffset" }
    | JDateTimeOffsetOption ->  { TypeId = None; Name = name |> fixName; Type = "DateTimeOffset option" }
    | JStringOption ->          { TypeId = None; Name = name |> fixName; Type = "string option" }
    | JEmptyObjectOption ->     { TypeId = None; Name = name |> fixName; Type = "Object option" }
    | JEmptyObject ->           { TypeId = None; Name = name |> fixName; Type = "Object" }
    | JObject _ ->              { TypeId = Some ^ idGenerator (); Name = name |> fixName; Type = name }
    | JObjectOption _ ->        { TypeId = Some ^ idGenerator (); Name = name |> fixName; Type = sprintf "%s %s" name "option" }
    | JArray obj ->             { TypeId = Some ^ idGenerator (); Name = name |> fixName; Type = fieldHandler fixName idGenerator collectionGenerator name obj |> getName |> collectionGenerator }
    | JArrayOption obj ->       { TypeId = Some ^ idGenerator (); Name = name |> fixName; Type = fieldHandler fixName idGenerator collectionGenerator name obj |> getName |> collectionGenerator }
    | _ -> failwith "translateToString unexcpected"

let private typeHandler fixName (Some id) name fields = { Id = id; Name = name |> fixName; Fields = fields }

let public foldi fold first = List.fold(fun (prev,i) c -> (fold i prev c,i + 1)) (first,0) >> fst

let compareType opt value = opt |> Option.map ((=) value) |> Option.fold (&&) true

let private generateUniqueNames nameGenerator =
    List.groupBy(fun x -> x.Name)
    >> List.map snd
    >> List.map(function 
                | [_] as list -> list 
                | list -> list |> List.mapi (fun i x -> { x with Name = nameGenerator x.Name i }))
    >> List.collect id

let private renameAllField types =
    types |> List.map(fun x ->
                    { x with Fields = 
                                x.Fields 
                                |> List.map (fun field -> 
                                             match  types |> List.tryFind(fun q -> compareType field.TypeId q.Id) with
                                             | None -> field
                                             | Some value -> { field with Name = value.Name }) })

let private distinctType =
    List.groupBy (fun x -> x.Name, x.Fields)
    >> List.map snd
    >> List.collect id

let private idGenerator () = Guid.NewGuid().ToString();
                                            
let private buildTypes rootObjectName fixName collectionGenerator json =
    match rootObjectName with
    | "" -> JsonResult.Error "Roob object should be not empty"
    | rootObjectName ->
        let rec tailDeep acc jobjs =
            match jobjs with
            | [] -> acc
            | (name, id, (JObject list))::xs 
            | (name, id, (JObjectOption list))::xs ->
                let newType = 
                    list
                    |> List.distinctBy fst
                    |> List.map (fun (key, value) -> fieldHandler fixName idGenerator collectionGenerator key value, value)

                let newJobjs = 
                    newType 
                    |> List.map(fun (field, json) -> (field.Name, field.TypeId, extractObject json))
                    |> List.choose(fun (key, id, v) -> match v with Some j -> Some (key, id, j) | None -> None)
             
                tailDeep ((typeHandler fixName id name (newType |> List.map fst))::acc) (newJobjs @ xs)
            | _ -> failwith "unexpected"

        let types = tailDeep [] [castArray [rootObjectName |> fixName, json] |> fun (x, y) -> (x, Some ^ Guid.NewGuid().ToString(), y)]

        types 
        |> distinctType
        |> generateUniqueNames (sprintf "%s%d")
        |> renameAllField
        |> JsonResult.Ok

let generateRecords fixName rootObjectName collectionGenerator (str: string) =
    match parseJsonString str with
    | Success(result, _, _) -> buildTypes rootObjectName fixName collectionGenerator result        
    | Failure(errorMsg, _, _) -> JsonResult.Error ^ errorMsg