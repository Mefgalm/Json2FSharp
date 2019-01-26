[<RequireQualifiedAccess>]
module FsharpCommon

open System
open System.Text.RegularExpressions

let listGenerator = sprintf "%s list"
let arrayGenerator = sprintf "%s array"
let charpListGenerator = sprintf "List<%s>"

let fixName (name: string) =
    let getFirstChar (name: string) = name.Chars 0
    let toUpperFirst name = ((Char.ToUpper (getFirstChar name)) |> Char.ToString) + name.Substring 1
    let newFieldName = Regex.Replace(name, "[!@#$%^&*()\-=+|/><\[\]\.\\*`]+", "") |> toUpperFirst

    if (not <| Char.IsLetter (getFirstChar name)) && getFirstChar name <> '_' then
        "The" + newFieldName
    else
        newFieldName