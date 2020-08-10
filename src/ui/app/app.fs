module Aornota.Cribbage.Ui.App

open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.GameEngine
open Aornota.Cribbage.Domain.Strategy

open Elmish
open Elmish.React

open Fable.MaterialUI.MaterialDesignIcons

open Fable.React.Adaptive
module ReactHB = Fable.React.HookBindings

open Feliz
open Feliz.MaterialUI

open FSharp.Data.Adaptive

let [<Literal>] private CRIBBAGE = "cribbage"

// *α* | β | γ | δ | ε | ζ | η | θ | ι | κ | λ | μ | ν | ξ | ο | π | ρ | σ | τ | υ | φ | χ | ψ | ω
let [<Literal>] private CRIBBAGE_VERSION = "α" // note: keep synchronized with  ./index.html | ../../package.json | ../../README.md

let [<Literal>] private CRIBBAGE_LOGO = "tpoc-32x32.png"

let [<Literal>] private CRIBBAGE_REPO = "https://github.com/aornota/cribbage"

type private PlayerDetails = {
    Player : Player
    Name : string
    ForCribStrategy : ForCribStrategy
    PegStrategy : PegStrategy }

type private Msg = | SettingsMsg of Settings.Msg

type private State = { Settings : Settings.State }

let private init () = { Settings = Settings.init () }, Cmd.none

let private transition msg state =
    match msg with
    | SettingsMsg msg ->
        let newSettings, cmd = Settings.transition msg state.Settings
        { state with Settings = newSettings }, cmd |> Cmd.map SettingsMsg

let private scores' = React.functionComponent (fun (input:{| scores : aval<int<point> * int<point>> ; player1 : PlayerDetails ; player2 : PlayerDetails |}) ->
    let player1Score, player2Score = ReactHB.Hooks.useAdaptive (input.scores)
    Mui.typography [
        typography.paragraph true
        typography.children [
            Html.strong input.player1.Name
            Html.text (sprintf " %i - %i "player1Score player2Score)
            Html.strong input.player2.Name ] ])
let private scores (engine:GameEngine) player1 player2 = scores' {| scores = engine.Scores ; player1 = player1 ; player2 = player2 |}

let private crib' = React.functionComponent (fun (input:{| crib : aval<CardS option> |}) ->
    let crib = ReactHB.Hooks.useAdaptive (input.crib)
    match crib with
    | Some cards ->
        Mui.typography [
            typography.children [
                Html.strong "Crib"
                Html.text (sprintf " -> %A" (cardsText cards)) ] ]
    | None -> Html.none)
let private crib (engine:GameEngine) = crib' {| crib = engine.Crib |}

let private cutCard' = React.functionComponent (fun (input:{| cutCard : aval<Card option> |}) ->
    let cutCard = ReactHB.Hooks.useAdaptive (input.cutCard)
    match cutCard with
    | Some card ->
        Mui.typography [
            typography.children [
                Html.strong "Cut card"
                Html.text (sprintf " -> %A" (cardText card)) ] ]
    | None -> Html.none)
let private cutCard (engine:GameEngine) = cutCard' {| cutCard = engine.CutCard |}

let [<Literal>] private SLEEP = 100

let private awaitingForCrib' = React.functionComponent (fun (input:{| awaitingForCrib : aval<(IsDealer * Hand * (CardS -> unit)) option> ; player : PlayerDetails |}) ->
    let awaitingForCrib = ReactHB.Hooks.useAdaptive (input.awaitingForCrib)
    let forCrib opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some (isDealer, hand, forCrib) -> forCrib (input.player.ForCribStrategy (isDealer, hand)) | None -> () }
    React.useEffect (forCrib awaitingForCrib >> Async.StartImmediate, [| box awaitingForCrib |])
    match awaitingForCrib with
    | Some (isDealer, hand, _) ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingForCrib (%s)" input.player.Name)
                Html.text (sprintf " -> %b | %A..." isDealer hand) ] ]
    | None -> Html.none)
let private awaitingForCrib (engine:GameEngine) player = awaitingForCrib' {| awaitingForCrib = engine.AwaitingForCrib(player.Player) ; player = player |}

let private awaitingPeg' = React.functionComponent (fun (input:{| awaitingPeg : aval<(PegState * (Card option -> unit)) option> ; player : PlayerDetails |}) ->
    let awaitingPeg = ReactHB.Hooks.useAdaptive (input.awaitingPeg)
    let peg opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some (pegState, peg) -> peg (input.player.PegStrategy pegState) | None -> () }
    React.useEffect (peg awaitingPeg >> Async.StartImmediate, [| box awaitingPeg |])
    match awaitingPeg with
    | Some (pegState, _) ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingPeg (%s)" input.player.Name)
                Html.text (sprintf " -> %A..." pegState) ] ]
    | None -> Html.none)
let private awaitingPeg (engine:GameEngine) player = awaitingPeg' {| awaitingPeg = engine.AwaitingPeg(player.Player) ; player = player |}

let private awaitingCannotPeg' = React.functionComponent (fun (input:{| awaitingCannotPeg : aval<(unit -> unit) option> ; player : PlayerDetails |}) ->
    let awaitingCannotPeg = ReactHB.Hooks.useAdaptive (input.awaitingCannotPeg)
    let cannotPeg opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some cannotPeg -> cannotPeg () | None -> () }
    React.useEffect (cannotPeg awaitingCannotPeg >> Async.StartImmediate, [| box awaitingCannotPeg |])
    match awaitingCannotPeg with
    | Some _ ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingCannotPeg (%s)..." input.player.Name) ] ]
    | None -> Html.none)
let private awaitingCannotPeg (engine:GameEngine) player = awaitingCannotPeg' {| awaitingCannotPeg = engine.AwaitingCannotPeg(player.Player) ; player = player |}

let private awaitingNewDeal' = React.functionComponent (fun (input:{| awaitingNewDeal : aval<(unit -> unit) option> ; player : PlayerDetails |}) ->
    let awaitingNewDeal = ReactHB.Hooks.useAdaptive (input.awaitingNewDeal)
    let newDeal opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some newDeal -> newDeal () | None -> () }
    React.useEffect (newDeal awaitingNewDeal >> Async.StartImmediate, [| box awaitingNewDeal |])
    match awaitingNewDeal with
    | Some _ ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingNewDeal (%s)..." input.player.Name) ] ]
    | None -> Html.none)
let private awaitingNewDeal (engine:GameEngine) player = awaitingNewDeal' {| awaitingNewDeal = engine.AwaitingNewDeal(player.Player) ; player = player |}

let private awaitingNewGame' = React.functionComponent (fun (input:{| engine : GameEngine |}) ->
    let awaitingNewGame = ReactHB.Hooks.useAdaptive (input.engine.AwaitingNewGame(Player1))
    match awaitingNewGame with
    | Some _ ->
        input.engine.Quit(Player1)
        Mui.typography [
            typography.children [
                Html.em "awaitingNewGame: will quit..." ] ]
    | None -> Html.none)
let private awaitingNewGame (engine:GameEngine) = awaitingNewGame' {| engine = engine |}

let private appBar' = React.functionComponent (fun (input:{| useDarkThemeSetting : bool option ; dispatch : Msg -> unit |}) ->
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
                            match input.useDarkThemeSetting with
                            | Some true -> "Using dark theme"
                            | Some false -> "Using light theme"
                            | None -> "Using system light/dark theme")
                        tooltip.children (
                            Mui.iconButton [
                                prop.onClick (fun _ -> Settings.ToggleUseDarkTheme |> SettingsMsg |> input.dispatch)
                                iconButton.color.inherit'
                                iconButton.children [
                                    match input.useDarkThemeSetting with
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
let private appBar useDarkThemeSetting dispatch = appBar' {| useDarkThemeSetting = useDarkThemeSetting ; dispatch = dispatch |}

let private render' = React.functionComponent (fun (input:{| state : State ; dispatch : Msg -> unit ; engine : GameEngine ; player1 : PlayerDetails ; player2 : PlayerDetails |}) ->
    let prefersDarkTheme = Hooks.useMediaQuery "@media (prefers-color-scheme: dark)"
    let useDarkTheme = input.state.Settings.UseDarkTheme |> Option.defaultValue prefersDarkTheme
    let theme = Theme.getTheme useDarkTheme
    let c = Theme.useStyles ()
    Mui.themeProvider [
        themeProvider.theme theme
        themeProvider.children [
            Html.div [
                prop.className c.root
                prop.children [
                    Mui.cssBaseline []
                    appBar input.state.Settings.UseDarkTheme input.dispatch
                    Html.main [
                        prop.className c.content
                        prop.children [
                            Mui.card [
                                card.classes.root c.contentCard
                                card.raised true
                                prop.children [
                                    scores input.engine input.player1 input.player2
                                    //crib input.engine
                                    //cutCard input.engine
                                    awaitingForCrib input.engine input.player1
                                    awaitingForCrib input.engine input.player2
                                    awaitingPeg input.engine input.player1
                                    awaitingPeg input.engine input.player2
                                    awaitingCannotPeg input.engine input.player1
                                    awaitingCannotPeg input.engine input.player2
                                    awaitingNewDeal input.engine input.player1
                                    awaitingNewDeal input.engine input.player2
                                    awaitingNewGame input.engine
                    ] ] ] ] ] ] ] ])

let private player1 = { Player = Player1 ; Name = "Basic" ; ForCribStrategy = forCribBasic ; PegStrategy = pegBasic }
let private player2 = { Player = Player2 ; Name = "Neph" ; ForCribStrategy = forCribIntermediate ; PegStrategy = pegIntermediate }

// TODO-NMB: Use "web worker"/s [see https://shmew.github.io/Feliz.UseWorker/] to run forCrib | peg strategies (since forCrib can take a couple of seconds, during which the UI is unresponsive)...
let private engine = GameEngine(player1.Name, player2.Name)

let private render state dispatch = render' {| state = state ; dispatch = dispatch ; engine = engine ; player1 = player1 ; player2 = player2 |}

Program.mkProgram init transition render
|> Program.withReactSynchronous "app" // needs to match id of div in index.html
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.run
