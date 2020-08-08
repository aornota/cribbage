module Aornota.Cribbage.Ui.App

open Aornota.Cribbage.Domain.GameEngine
open Aornota.Cribbage.Domain.Strategy

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

type Msg = | SettingsMsg of Settings.Msg

type State = { Settings : Settings.State }

let private init () = { Settings = Settings.init () }, Cmd.none

let private transition msg state =
    match msg with
    | SettingsMsg msg ->
        let newSettings, cmd = Settings.transition msg state.Settings
        { state with Settings = newSettings }, cmd |> Cmd.map SettingsMsg

let private render' = React.functionComponent (fun (input: {| state : State ; dispatch : Msg -> unit |}) ->
    let theme, styles = Theme.getTheme input.state.Settings.UseDarkTheme, Theme.useStyles ()
    Mui.themeProvider [
        themeProvider.theme theme
        themeProvider.children [
            Html.div [
                prop.className styles.root
                prop.children [
                    Mui.cssBaseline []
                    Mui.appBar [
                        prop.className styles.appBar
                        appBar.color.secondary
                        appBar.position.fixed'
                        appBar.children [
                            Mui.toolbar [
                                toolbar.children [
                                    Mui.typography [
                                        prop.className styles.title
                                        typography.align.left
                                        typography.children [
                                            Html.img [
                                                prop.style [ style.verticalAlign.middle ]
                                                prop.src CRIBBAGE_LOGO
                                                prop.alt CRIBBAGE ]
                                            Html.text " | "
                                            Html.strong CRIBBAGE
                                            Html.text (sprintf " (%s)" CRIBBAGE_VERSION) ] ]
                                    Mui.iconButton [
                                        prop.className styles.titleButton
                                        iconButton.disableRipple true
                                        prop.onClick <| fun _ -> Settings.ToggleUseDarkTheme |> SettingsMsg |> input.dispatch
                                        prop.children [ themeLightDarkIcon [] ] ]
                                    Mui.iconButton [
                                        prop.className styles.titleButton
                                        prop.href "https://github.com/aornota/cribbage"
                                        prop.custom ("target", "_blank")
                                        iconButton.disableRipple true
                                        prop.children [ githubCircleIcon [] ] ] ] ] ] ]
                    (* Html.main [
                        prop.className c.content
                        prop.children [
                            Mui.card [
                                prop.className c.contentCard
                                card.raised true
                                prop.children (content input.model) ] ] ] *)
                ] ] ] ])

let private render (state:State) dispatch = render' {| state = state ; dispatch = dispatch |}

// TODO-NMB: Figure out why this is problematic...GameEngine(Computer ("Basic 1", forCribBasic, pegBasic), Computer ("Basic 2", forCribBasic, pegBasic)) |> ignore

Program.mkProgram init transition render
|> Program.withReactSynchronous "app" // needs to match id of div in index.html
|> Program.withConsoleTrace
|> Program.run
