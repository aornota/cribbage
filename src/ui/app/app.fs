module Aornota.Cribbage.Ui.App

open Browser.Dom

open Fable.MaterialUI.MaterialDesignIcons

open Fable.React.Adaptive
module ReactHB = Fable.React.HookBindings

open Feliz
open Feliz.MaterialUI

let [<Literal>] private CRIBBAGE = "cribbage"

// *pre-α" | α | β | γ | δ | ε | ζ | η | θ | ι | κ | λ | μ | ν | ξ | ο | π | ρ | σ | τ | υ | φ | χ | ψ | ω
let [<Literal>] private CRIBBAGE_VERSION = "α" // note: keep synchronized with  ./index.html | ../../package.json | ../../README.md

let [<Literal>] private CRIBBAGE_LOGO = "tpoc-32x32.png"

let [<Literal>] private CRIBBAGE_REPO = "https://github.com/aornota/cribbage"

let private appBar' = React.functionComponent ("AppBar", fun (props:{| useDarkThemeSetting : bool option ; prefersDarkTheme : bool |}) ->
    let c = Theme.useStyles ()
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
                            match props.useDarkThemeSetting with
                            | Some true -> "Using dark theme"
                            | Some false -> "Using light theme"
                            | None -> "Using default theme")
                        tooltip.children (
                            Mui.iconButton [
                                prop.onClick (fun _ -> Settings.toggleUseDarkTheme props.prefersDarkTheme)
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
                                iconButton.component' "a"
                                iconButton.color.inherit'
                                iconButton.children [ githubCircleIcon [] ] ]) ] ] ] ] ])
let private appBar (useDarkThemeSetting, prefersDarkTheme) = appBar' {| useDarkThemeSetting = useDarkThemeSetting ; prefersDarkTheme = prefersDarkTheme |}

let private app = React.functionComponent ("App", fun () ->
    let prefersDarkTheme = Hooks.useMediaQuery "@media (prefers-color-scheme: dark)"
    let useDarkThemeSetting = ReactHB.Hooks.useAdaptive (Settings.useDarkTheme)
    let useDarkTheme = useDarkThemeSetting |> Option.defaultValue prefersDarkTheme
    let theme = Theme.getTheme useDarkTheme
    let c = Theme.useStyles ()
    Mui.themeProvider [
        themeProvider.theme theme
        themeProvider.children [
            Html.div [
                prop.className c.root
                prop.children [
                    Mui.cssBaseline []
                    appBar (useDarkThemeSetting, prefersDarkTheme)
                    Html.main [
                        prop.className c.content
                        prop.children [
                            Game.game () ] ] ] ] ] ])

ReactDOM.render (app (), document.getElementById "app") // needs to match id of div in index.html
