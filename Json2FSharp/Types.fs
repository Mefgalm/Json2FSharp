module Types

type Field =
    { Name: string
      Type: string }

type Type =
    { Name: string
      Fields: Field list }

type JsonResult<'a> =
    | Ok of 'a
    | Error of string

type Json = 
    | JBool
    | JBoolOption
    | JNull
    | JInt
    | JIntOption
    | JFloat
    | JFloatOption
    | JString 
    | JDateTimeOffset
    | JDateTimeOffsetOption
    | JStringOption
    | JList of Json list
    | JEmptyObject
    | JEmptyObjectOption
    | JObject of (string * Json) list
    | JObjectOption of (string * Json) list
    | JArray of Json
    | JArrayOption of Json