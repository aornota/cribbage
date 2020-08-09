module Aornota.Cribbage.Ui.App

open Aornota.Cribbage.Domain.GameEngine
//open Aornota.Cribbage.Domain.Strategy

open Elmish
open Elmish.React

open Fable.MaterialUI.MaterialDesignIcons

(* open Fable.React.Adaptive
module ReactHB = Fable.React.HookBindings *)

open Feliz
open Feliz.MaterialUI

(* open FSharp.Data.Adaptive *)

let [<Literal>] private CRIBBAGE = "cribbage"

// *α* | β | γ | δ | ε | ζ | η | θ | ι | κ | λ | μ | ν | ξ | ο | π | ρ | σ | τ | υ | φ | χ | ψ | ω
let [<Literal>] private CRIBBAGE_VERSION = "α" // note: keep synchronized with  ./index.html | ../../package.json | ../../README.md

let [<Literal>] private CRIBBAGE_LOGO = "tpoc-32x32.png"

let [<Literal>] private CRIBBAGE_REPO = "https://github.com/aornota/cribbage"

type Msg = | SettingsMsg of Settings.Msg

type State = { Settings : Settings.State }

let private init () = { Settings = Settings.init () }, Cmd.none

let private transition msg state =
    match msg with
    | SettingsMsg msg ->
        let newSettings, cmd = Settings.transition msg state.Settings
        { state with Settings = newSettings }, cmd |> Cmd.map SettingsMsg

let private render' = React.functionComponent (fun (input: {| state : State ; dispatch : Msg -> unit |}) ->
    let prefersDarkTheme = Hooks.useMediaQuery "@media (prefers-color-scheme: dark)"
    let useDarkTheme = input.state.Settings.UseDarkTheme |> Option.defaultValue prefersDarkTheme
    let theme, c = Theme.getTheme useDarkTheme, Theme.useStyles ()
    Mui.themeProvider [
        themeProvider.theme theme
        themeProvider.children [
            Html.div [
                prop.className c.root
                prop.children [
                    Mui.cssBaseline []
                    Mui.appBar [
                        appBar.classes.root c.appBar
                        appBar.color.secondary
                        appBar.position.fixed'
                        appBar.children [
                            Mui.toolbar [
                                toolbar.classes.root c.toolbar
                                toolbar.children [
                                    Mui.typography [
                                        typography.classes.root c.title
                                        typography.variant.h6
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
                                            match input.state.Settings.UseDarkTheme with
                                            | Some true -> "Using dark theme"
                                            | Some false -> "Using light theme"
                                            | None -> "Using system light/dark theme")
                                        tooltip.children (
                                            Mui.iconButton [
                                                prop.onClick (fun _ -> Settings.ToggleUseDarkTheme |> SettingsMsg |> input.dispatch)
                                                iconButton.color.inherit'
                                                iconButton.children [
                                                    match input.state.Settings.UseDarkTheme with
                                                    | Some true -> brightness4Icon []
                                                    | Some false -> brightness7Icon  []
                                                    | None -> brightnessAutoIcon [] ] ] ) ]
                                    Mui.tooltip [
                                        tooltip.title "aornota/cribbage on GitHub"
                                        tooltip.children (
                                            Mui.iconButton [
                                                prop.href CRIBBAGE_REPO
                                                prop.custom ("target", "_blank")
                                                //iconButton.component' "a"
                                                iconButton.color.inherit'
                                                iconButton.children [ githubCircleIcon [] ] ]) ] ] ] ] ]
                    (* Html.main [
                        prop.className c.content
                        prop.children [
                            Mui.card [
                                prop.className c.contentCard
                                card.raised true
                                prop.children (content input.model) ] ] ] *)
                ] ] ] ])

let private render (state:State) dispatch = render' {| state = state ; dispatch = dispatch |}

// TODO-NMB: Use "web worker"/s [see https://shmew.github.io/Feliz.UseWorker/] to run forCrib | peg strategies (since forCrib can take a couple of seconds, during which the UI is unresponsive)...
GameEngine("Basic", "Neph") |> ignore

Program.mkProgram init transition render
|> Program.withReactSynchronous "app" // needs to match id of div in index.html
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.run
