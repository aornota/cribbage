[<RequireQualifiedAccessAttribute>]
module Aornota.Cribbage.Ui.Toaster

open Aornota.Cribbage.Domain.GameEngine

open Elmish

open Feliz
open Feliz.MaterialUI
open Fable.MaterialUI.MaterialDesignIcons

open System

open Thoth.Elmish

type Icon = | Computer | Human | Score // TODO-NMB: More?...

type Purpose = | Information | Success | Warning | Error

type ToastData = {
    Title : string option
    Icon : Icon option
    Message : string
    Purpose : Purpose
    Affinity : Player option
    TimeoutOverride : float option }

let [<Literal>] private DEFAULT_TIMEOUT = 3.0

let private iconElement = function | Computer -> robotIcon [] | Human -> humanIcon [] | Score -> counterIcon []

let private position = function | Some Player1 -> Toast.TopRight | Some Player2 -> Toast.BottomRight | None -> Toast.TopCenter

let private toastCmd toCmd data : Cmd<_> =
    let toast = Toast.message data.Message
    let toast = match data.Title with | Some title -> toast |> Toast.title title | None -> toast
    let toast = match data.Icon with | Some icon -> toast |> Toast.icon (iconElement icon) | None -> toast
    toast
    |> Toast.position (position data.Affinity)
    |> Toast.timeout (TimeSpan.FromSeconds (data.TimeoutOverride |> Option.defaultValue DEFAULT_TIMEOUT |> float))
    |> Toast.dismissOnClick
    |> toCmd

let makeCmd data =
    match data.Purpose with | Information -> toastCmd Toast.info data | Success -> toastCmd Toast.success data | Warning -> toastCmd Toast.warning data | Error -> toastCmd Toast.error data

// Adapted from https://github.com/MangelMaxime/Thoth/blob/master/src/Thoth.Elmish.Toast/Toast.fs#L448-L494.
let render = {
    new Toast.IRenderer<ReactElement> with
    member __.Toast children _ =
        Html.div [
            prop.className "toast"
            prop.children children ]
    member __.CloseButton onClick =
        Html.span [
            prop.className "close-button"
            prop.onClick onClick ]
    member __.InputArea _ = Html.div [ Html.strong "InputArea has not been implemented" ]
    member __.Input _ _ = Html.div [ Html.strong "Input has not been implemented" ]
    member __.Title title =
        Mui.typography [
            typography.variant.body1
            typography.children [
                Html.strong title ] ]
    member __.Icon iconElement =
        Html.div [
            prop.className "toast-layout-icon"
            prop.children [ iconElement ] ]
    member __.SingleLayout titleElement messageElement =
        Html.div [
            prop.className "toast-layout-content"
            prop.children [
                titleElement
                messageElement ] ]
    member __.Message message =
        Mui.typography [
            typography.variant.body2
            typography.children [
                Html.text message ] ]
    member __.SplittedLayout iconElement titleElement messageElement =
        Html.div [
            prop.style [
                style.display.flex
                style.width (length.percent 100) ]
            prop.children [
                iconElement
                Html.div [
                    prop.className "toast-layout-content"
                    prop.children [
                        titleElement
                        messageElement ] ] ] ]
    member __.StatusToColor status =
        match status with
        | Toast.Success -> "is-success"
        | Toast.Warning -> "is-warning"
        | Toast.Error -> "is-error"
        | Toast.Info -> "is-info" }
