module Aornota.Cribbage.Ui.App

open Elmish
open Elmish.React

open Fable.MaterialUI.MaterialDesignIcons

open Fable.React.Adaptive
module ReactHB = Fable.React.HookBindings

open Feliz
open Feliz.MaterialUI

open Thoth.Elmish

type private Msg = | ShowToast of Toaster.ToastData

type private State = unit

let [<Literal>] private CRIBBAGE = "cribbage"

// *pre-α" | α | β | γ | δ | ε | ζ | η | θ | ι | κ | λ | μ | ν | ξ | ο | π | ρ | σ | τ | υ | φ | χ | ψ | ω
let [<Literal>] private CRIBBAGE_VERSION = "pre-α" // note: keep synchronized with  ./index.html | ../../package.json | ../../README.md

let [<Literal>] private CRIBBAGE_LOGO = "tpoc-32x32.png"

let [<Literal>] private CRIBBAGE_REPO = "https://github.com/aornota/cribbage"

let private init () : State * Cmd<Msg> = (), Cmd.none

let private transition msg state : State * Cmd<Msg> = match msg with | ShowToast data -> state, Toaster.makeCmd data

let private appBar' = React.functionComponent ("AppBar", fun (props:{| useDarkThemeSetting : bool option ; prefersDarkTheme : bool |}) ->
    Mui.appBar [
        appBar.color.default'
        appBar.position.fixed'
        prop.style [ style.display.grid ; style.cursor "default" ]
        appBar.children [
            Mui.toolbar [
                toolbar.children [
                    Mui.typography [
                        typography.variant.h6
                        prop.style [ style.width (length.percent 100) ]
                        typography.children [
                            Html.img [
                                prop.style [ style.verticalAlign.middle ]
                                prop.src CRIBBAGE_LOGO
                                prop.alt CRIBBAGE ]
                            Html.text " | "
                            Html.strong CRIBBAGE
                            Html.text " ("
                            Html.em CRIBBAGE_VERSION
                            Html.text ")"  ] ]
                    Mui.tooltip [
                        tooltip.title (
                            match props.useDarkThemeSetting with
                            | Some true -> "Using dark theme"
                            | Some false -> "Using light theme"
                            | None -> "Using default theme")
                        tooltip.children (
                            Mui.iconButton [
                                prop.onClick (fun _ -> Storage.toggleUseDarkTheme props.prefersDarkTheme)
                                iconButton.color.inherit'
                                iconButton.children [
                                    match props.useDarkThemeSetting with
                                    | Some true -> brightness4Icon []
                                    | Some false -> brightness7Icon  []
                                    | None -> brightnessAutoIcon [] ] ] ) ]
                    Mui.tooltip [
                        tooltip.title "aornota/cribbage on GitHub"
                        tooltip.children (
                            Mui.iconButton [
                                prop.href CRIBBAGE_REPO
                                prop.custom ("target", "_blank")
                                prop.custom ("rel", "noopener")
                                iconButton.component' "a"
                                iconButton.color.inherit'
                                iconButton.children [ githubCircleIcon [] ] ]) ] ] ] ] ])
let private appBar (useDarkThemeSetting, prefersDarkTheme) = appBar' {| useDarkThemeSetting = useDarkThemeSetting ; prefersDarkTheme = prefersDarkTheme |}

let private app' = React.functionComponent ("App", fun (props:{| dispatch : Msg -> unit |}) ->
    let prefersDarkTheme = Hooks.useMediaQuery "@media (prefers-color-scheme: dark)"
    let useDarkThemeSetting = ReactHB.Hooks.useAdaptive Storage.useDarkTheme
    let useDarkTheme = useDarkThemeSetting |> Option.defaultValue prefersDarkTheme
    let theme = Theme.getTheme useDarkTheme
    Mui.themeProvider [
        themeProvider.theme theme
        themeProvider.children [
            Html.div [
                prop.style [ style.display.flex ; style.height.inheritFromParent ; style.userSelect.none ]
                prop.children [
                    Mui.cssBaseline []
                    appBar (useDarkThemeSetting, prefersDarkTheme)
                    Html.main [
                        prop.style [
                            style.height.inheritFromParent
                            style.flexGrow 1
                            style.paddingTop (length.em 6)
                            style.paddingLeft (length.em 2)
                            style.paddingRight (length.em 2)
                            style.paddingBottom (length.em 1) ]
                        prop.children [
                            // TEMP-NMB...Game.game (React.useCallback (ShowToast >> props.dispatch)) ] ] ] ] ] ])
                            Game.game (React.useCallback ignore) ] ] ] ] ] ])
let private app dispatch = app' {| dispatch = dispatch |}

let private render (_:State) (dispatch:Msg -> unit) = app dispatch

Program.mkProgram init transition render
|> Toast.Program.withToast Toaster.render
|> Program.withReactSynchronous "app" // needs to match id of div in index.html
#if CONSOLE_TRACE
|> Program.withConsoleTrace
#endif
|> Program.run
