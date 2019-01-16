// Learn more about F# at http://fsharp.org

open System
open FParsec
open Newtonsoft.Json.Linq

//[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Json = 
    | JBool
    | JBoolOption
    | JNull
    | JInt
    | JIntOption
    | JFloat
    | JFloatOption
    | JString 
    | JStringOption
    | JList of Json list
    | JEmptyObjectOption
    | JObject of (string * Json) list
    | JObjectOption of (string * Json) list
    | JArray of Json
    | JArrayOption of Json

let ws   = spaces // eats any whitespace
let str s = pstring s

let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    between (str "\"") (str "\"")
            (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                          (str "\\" >>. (escape <|> unicodeEscape)))

let jstring = stringLiteral |>> (fun _ -> JString)

let jnumber = pfloat |>> (fun _ -> JFloat)

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
                        jstring
                        jnumber
                        jtrue
                        jnull
                        jfalse]

let json = ws >>. jvalue .>> ws .>> eof

let parseJsonString str = run json str

let rec aggreagateListToSingleType jsonList =
    
    let (<||>) f1 f2 x = f1 x || f2 x
    let (<&&>) f1 f2 x = f1 x && f2 x

    let isArray = function JArray _ | JArrayOption _ -> true | _ -> false
    let isList = function JList _ -> true | _ -> false
    let isString = function JStringOption | JString -> true | _ -> false
    let isNumber = function JInt | JFloat| JIntOption | JFloatOption -> true | _ -> false
    let isNull = function JNull -> true | _ -> false
    let isBool = function JBool -> true | _ -> false
    let isObject = function JObject _ | JObjectOption _ -> true | _ -> false
    let isStringOption = function JStringOption -> true | _ -> false
    let isArrayOption = function JArrayOption _ -> true | _ -> false
    let isObjectOption = function JObjectOption _ -> true | _ -> false
    let isNumberOption = function JIntOption | JFloatOption -> true | _ -> false
    let hasNull = List.exists isNull
    let typeOrder = 
        function 
        | JInt | JIntOption -> 1 
        | JFloat | JFloatOption -> 2
        | _ -> failwith "Not number type"
    
    let checkStringOption = hasNull <||> List.exists isStringOption
    let checkArrayOption = hasNull <||> List.exists isArrayOption
    let checkObjectOption = hasNull <||> List.exists isObjectOption
    let checkNumberOption = hasNull <||> List.exists isNumberOption

    let getOptionType istanceType isOption =
        match istanceType, isOption with
        | JInt, true -> JIntOption
        | JFloat, true -> JFloatOption
        | JBool, true -> JBoolOption
        | JString, true -> JStringOption
        | JObject x, true -> JObjectOption x
        | JArray x, true -> JArrayOption x
        | x, _ -> x

    match jsonList with
    | [] -> JEmptyObjectOption
    | list when list |> List.forall isNull -> JEmptyObjectOption
    | list when list |> List.forall (isNumber <||> isNull) ->
        let newType = 
                list 
                |> List.filter (not << isNull)
                |> List.distinct
                |> List.map(fun x -> (x, typeOrder x))
                |> List.maxBy (fun (_, rank) -> rank)
                |> fst
        getOptionType newType (list |> checkNumberOption)

    | list when list |> List.forall (isString <||> isNull) ->
        getOptionType JString (list |> checkStringOption)

    | list when list |> List.forall (isObject <||> isNull) ->
        let getObjects = List.filter (not << isNull) >> List.map(function JObject list | JObjectOption list -> list)
        let res = 
            list 
            |> getObjects
            |> List.collect(fun x -> x)
            |> List.groupBy(fun (key, _) -> key)
            |> List.map(fun (key, value) -> (key, (aggreagateListToSingleType (value |> List.map snd))))
        getOptionType (JObject res) (list |> checkObjectOption)

    | list when list |> List.forall (isList <||> isNull) ->
        let getLists = List.filter (not << isNull) >> List.map(fun (JList x) -> x)

        let res =
            list 
            |> getLists
            |> List.map aggreagateListToSingleType            
            |> aggreagateListToSingleType
            
        getOptionType (JArray res) (list |> checkArrayOption)    

    | list when list |> List.forall (isArray <||> isNull) ->
        let getObjs = List.filter (not << isNull) >> List.map(function JArray list | JArrayOption list -> list)

        let res =
            list 
            |> getObjs
            |> aggreagateListToSingleType

        getOptionType (JArray res) (list |> checkArrayOption)

    | _ -> JEmptyObjectOption

let rec test2 acc (nodes: (string * Json) list) = 
    match nodes with
    | [] -> acc
    | (objName, JObject list)::xs -> 

        let getType name =
            function 
            | JString -> "string"
            | JFloat -> "float"
            | JBool -> "bool"
            | JNull -> "object"
            | JObject _ -> name
            | JList [] -> "object list"
            | JList _ -> name + " list"

        let newType name = 
             List.map(fun (x, y) -> x + ": " + getType x y)
             >> List.map(fun x -> "\t" + x + "\n")                                  
             >> List.reduce(+)
             >> fun x -> name + "\n" + x
        
        let newNodes =
             List.map(fun (n, v) -> 
                      match v with 
                      | JObject list -> Some (n, JObject list)
                      | JList list -> Some(n, aggreagateListToSingleType list)
                      | _ -> None)
             >> List.choose(fun x -> x)

        test2 ((newType objName list)::acc) (xs @ (newNodes list))
    | _ -> acc

[<EntryPoint>]
let main argv =
    
    let testExample = @"  {
             ""glossary"": {
                ""ggfd"": [],
                ""name"": ""vlad"",
                ""name2"": null,
                ""aaa"": {
                    ""test"": ""ttt""
                }
              }
            }"

    match parseJsonString testExample with
    | Success(result, _, _)   -> printfn "Success: %A" (test2 [] ["root", result])
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    Console.ReadKey() |> ignore
    0 // return an integer exit code
