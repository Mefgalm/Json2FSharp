module Types

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
    | JEmptyObjectOption
    | JObject of (string * Json) list
    | JObjectOption of (string * Json) list
    | JArray of Json
    | JArrayOption of Json