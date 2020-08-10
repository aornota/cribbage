[<RequireQualifiedAccess>]
module Aornota.Cribbage.Ui.Settings

// Adapted from on https://github.com/Shmew/Feliz.MaterialUI.MaterialTable/blob/master/docs/App.fs.

open Browser.WebStorage

open FSharp.Data.Adaptive

type Setting = | UseDarkTheme

let private readSetting setting parser =
    localStorage.getItem (string setting)
    |> function | value when isNull value -> None | value -> Some value
    |> Option.bind (fun value -> try parser value |> Some with _ -> None)

let private writeSetting setting value = localStorage.setItem (string setting, value)

let private asBool (s:string) = match s.ToLower() with | "true" -> true | _ -> false

let useDarkTheme = cval (readSetting UseDarkTheme asBool)

let toggleUseDarkTheme prefersDarkTheme =
    let newUseDarkTheme = match useDarkTheme.Value with | None -> not prefersDarkTheme | Some false -> true | Some true -> false
    writeSetting UseDarkTheme (string newUseDarkTheme)
    transact (fun _ -> useDarkTheme.Value <- Some newUseDarkTheme)
