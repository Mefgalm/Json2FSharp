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
    | EmptyList -> JEmptyObjectOption
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

let rec castArray = 
     List.map(fun (key, value) ->
                match value with
                | JObject list -> key, JObject ^ castArray list
                | JList list -> key, JArray ^ aggreagateListToSingleType list
                | _ -> key, value)

let rec extractObject json =
    match json with
    | JArray obj 
    | JArrayOption obj -> extractObject obj
    | JObject _ 
    | JObjectOption _  -> Some json
    | _ -> None

let rec deep fieldHandler typeHandler toView node =
    let rec tailDeep acc jobjs =
        match jobjs with
        | [] -> acc
        | (name, (JObject list))::xs 
        | (name, (JObjectOption list))::xs ->
            let newType = 
                list
                |> List.distinctBy (fun (name, _) -> name)
                |> List.map (fun (key, value) -> fieldHandler key value)
                |> fun x -> typeHandler name x

            let newJobjs = 
                list 
                |> List.map(fun (key, value) -> (key, extractObject value))
                |> List.choose(fun (key, v) -> match v with Some j -> Some (key, j) | None -> None)
                       
            tailDeep (newType::acc) (newJobjs @ xs)
        | _ -> failwith "unexpected"

    tailDeep [] [node] |> toView