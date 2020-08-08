[<RequireQualifiedAccess>]
module Aornota.Cribbage.Ui.Settings

// Based on https://github.com/Shmew/Feliz.MaterialUI.MaterialTable/blob/master/docs/App.fs.

open Browser.WebStorage

open Elmish

type Setting = | UseDarkTheme

type Msg = | ToggleUseDarkTheme

type State = { UseDarkTheme : bool }

let private readSetting setting parser defaultValue =
    localStorage.getItem (string setting)
    |> function | value when isNull value -> None | value -> Some value
    |> Option.bind (fun value -> try parser value |> Some with _ -> None)
    |> Option.defaultValue defaultValue

let private writeSetting setting value = localStorage.setItem (string setting, value)

let private asBool (s:string) = match s.ToLower() with | "true" -> true | _ -> false

let init () = { UseDarkTheme = readSetting UseDarkTheme asBool false }

let transition msg state =
    match msg with
    | ToggleUseDarkTheme ->
        let newUseDarkTheme = not state.UseDarkTheme
        writeSetting UseDarkTheme (string newUseDarkTheme)
        { state with UseDarkTheme = newUseDarkTheme }, Cmd.none
