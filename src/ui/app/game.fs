[<RequireQualifiedAccess>]
module Aornota.Cribbage.Ui.Game

open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.GameEngine
open Aornota.Cribbage.Domain.Strategy

open Fable.React.Adaptive
module ReactHB = Fable.React.HookBindings

open Feliz
open Feliz.MaterialUI

open FSharp.Data.Adaptive

type private PlayerDetails = { // TODO-NMB: Distinguish between non-interactive (with forCrib | peg strategies) and interactive?...
    Name : string
    ForCribStrategy : ForCribStrategy
    PegStrategy : PegStrategy }

type private GameDetails = { // TODO-NMB: More (cf. dev-console\game-player.fs)?...
    Player1Details : PlayerDetails
    Player2Details : PlayerDetails
    GameEngine : GameEngine }

let [<Literal>] private SLEEP = 1

// TODO-NMB: Use "web worker"/s [see https://shmew.github.io/Feliz.UseWorker/] to run forCrib | peg strategies (since forCrib can take a couple of seconds, during which the UI is unresponsive)...

// TODO-NMB: Should scores | awaitingForCrib | &c. be React.memo?...

let private scores' = React.functionComponent ("Scores", fun (props:{| scores : aval<int<point> * int<point>> ; player1 : PlayerDetails ; player2 : PlayerDetails |}) ->
    let player1Score, player2Score = ReactHB.Hooks.useAdaptive (props.scores)
    Mui.typography [
        typography.paragraph true
        typography.children [
            Html.strong props.player1.Name
            Html.text (sprintf " %i - %i "player1Score player2Score)
            Html.strong props.player2.Name ] ])
let private scores (scores, player1, player2) = scores' {| scores = scores ; player1 = player1 ; player2 = player2 |}

let private crib' = React.functionComponent ("Crib", fun (props:{| crib : aval<CardS option> |}) ->
    let crib = ReactHB.Hooks.useAdaptive (props.crib)
    match crib with
    | Some cards ->
        Mui.typography [
            typography.children [
                Html.strong "Crib"
                Html.text (sprintf " -> %A" (cardsText cards)) ] ]
    | None -> Html.none)
let private crib crib = crib' {| crib = crib |}

let private cutCard' = React.functionComponent ("CutCard", fun (props:{| cutCard : aval<Card option> |}) ->
    let cutCard = ReactHB.Hooks.useAdaptive (props.cutCard)
    match cutCard with
    | Some card ->
        Mui.typography [
            typography.children [
                Html.strong "Cut card"
                Html.text (sprintf " -> %A" (cardText card)) ] ]
    | None -> Html.none)
let private cutCard cutCard = cutCard' {| cutCard = cutCard |}

let private awaitingForCrib' = React.functionComponent ("AwaitingForCrib", fun (props:{| fForCrib : aval<(IsDealer * Hand * (CardS -> unit)) option> ; player : PlayerDetails |}) ->
    let fForCrib = ReactHB.Hooks.useAdaptive (props.fForCrib)
    let forCrib opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some (isDealer, hand, forCrib) -> forCrib (props.player.ForCribStrategy (isDealer, hand)) | None -> () }
    React.useEffect (forCrib fForCrib >> Async.StartImmediate, [| box fForCrib |])
    match fForCrib with
    | Some (isDealer, hand, _) ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingForCrib (%s)" props.player.Name)
                Html.text (sprintf " -> %b | %A..." isDealer hand) ] ]
    | None -> Html.none)
let private awaitingForCrib (fForCrib, player) = awaitingForCrib' {| fForCrib = fForCrib ; player = player |}

let private awaitingPeg' = React.memo ("AwaitingPeg", fun (props:{| fPeg : aval<(PegState * (Card option -> unit)) option> ; player : PlayerDetails |}) ->
    let fPeg = ReactHB.Hooks.useAdaptive (props.fPeg)
    let peg opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some (pegState, peg) -> peg (props.player.PegStrategy pegState) | None -> () }
    React.useEffect (peg fPeg >> Async.StartImmediate, [| box fPeg |])
    match fPeg with
    | Some (pegState, _) ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingPeg (%s)" props.player.Name)
                Html.text (sprintf " -> %A..." pegState) ] ]
    | None -> Html.none)
let private awaitingPeg (fPeg, player) = awaitingPeg' {| fPeg = fPeg ; player = player |}

let private awaitingCannotPeg' = React.functionComponent ("AwaitingCannotPeg", fun (props:{| fCannotPeg : aval<(unit -> unit) option> ; player : PlayerDetails |}) ->
    let fCannotPeg = ReactHB.Hooks.useAdaptive (props.fCannotPeg)
    let cannotPeg opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some cannotPeg -> cannotPeg () | None -> () }
    React.useEffect (cannotPeg fCannotPeg >> Async.StartImmediate, [| box fCannotPeg |])
    match fCannotPeg with
    | Some _ ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingCannotPeg (%s)..." props.player.Name) ] ]
    | None -> Html.none)
let private awaitingCannotPeg (fCannotPeg, player) = awaitingCannotPeg' {| fCannotPeg = fCannotPeg ; player = player |}

let private awaitingNewDeal' = React.functionComponent ("AwaitingNewDeal", fun (props:{| fNewDeal : aval<(unit -> unit) option> ; player : PlayerDetails |}) ->
    let fNewDeal = ReactHB.Hooks.useAdaptive (props.fNewDeal)
    let newDeal opt () = async {
        do! Async.Sleep SLEEP
        match opt with | Some newDeal -> newDeal () | None -> () }
    React.useEffect (newDeal fNewDeal >> Async.StartImmediate, [| box fNewDeal |])
    match fNewDeal with
    | Some _ ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingNewDeal (%s)..." props.player.Name) ] ]
    | None -> Html.none)
let private awaitingNewDeal (fNewDeal, player) = awaitingNewDeal' {| fNewDeal = fNewDeal ; player = player |}

let private awaitingNewGame' = React.functionComponent ("AwaitingNewGame", fun (props:{| engine : GameEngine |}) ->
    let awaitingNewGame = ReactHB.Hooks.useAdaptive (props.engine.AwaitingNewGame(Player1))
    match awaitingNewGame with
    | Some _ ->
        props.engine.Quit(Player1)
        Mui.typography [
            typography.children [
                Html.em "awaitingNewGame: will quit..." ] ]
    | None -> Html.none)
let private awaitingNewGame engine = awaitingNewGame' {| engine = engine |}

let game = React.memo ("Game", fun () ->
    let (gameDetails, setGameDetails) : GameDetails option * (GameDetails option -> unit) = React.useState (None)
    (* IMPORTANT NOTE: Do *not* call "Theme.useStyles ()" as this causes a re-render (despite memo-ization) when light/dark theme changed - which seems to screw up state / adaptive stuff. *)
    match gameDetails with
    | Some gameDetails ->
        // TODO-NMB: Rework for proper game UI...
        Mui.card [
            card.raised true
            prop.children [
                scores (gameDetails.GameEngine.Scores, gameDetails.Player1Details, gameDetails.Player2Details)
                //crib gameDetails.GameEngine.Crib
                //cutCard gameDetails.GameEngine.CutCard
                awaitingForCrib (gameDetails.GameEngine.AwaitingForCrib(Player1), gameDetails.Player1Details)
                awaitingForCrib (gameDetails.GameEngine.AwaitingForCrib(Player2), gameDetails.Player2Details)
                awaitingPeg (gameDetails.GameEngine.AwaitingPeg(Player1), gameDetails.Player1Details)
                awaitingPeg (gameDetails.GameEngine.AwaitingPeg(Player2), gameDetails.Player2Details)
                awaitingCannotPeg (gameDetails.GameEngine.AwaitingCannotPeg(Player1), gameDetails.Player1Details)
                awaitingCannotPeg (gameDetails.GameEngine.AwaitingCannotPeg(Player2), gameDetails.Player2Details)
                awaitingNewDeal (gameDetails.GameEngine.AwaitingNewDeal(Player1), gameDetails.Player1Details)
                awaitingNewDeal (gameDetails.GameEngine.AwaitingNewDeal(Player2), gameDetails.Player2Details)
                awaitingNewGame gameDetails.GameEngine
        ] ]
    | None ->
        // TODO-NMB: UI to enter name | select opponent skill level | &c. | ...
        let player1Details : PlayerDetails = { Name = "Random" ; ForCribStrategy = forCribRandom ; PegStrategy = pegRandom }
        let player2Details : PlayerDetails = { Name = "Neph" ; ForCribStrategy = forCribIntermediate ; PegStrategy = pegIntermediate }
        let gameDetails = {
            Player1Details = player1Details
            Player2Details = player2Details
            GameEngine = GameEngine (player1Details.Name, player2Details.Name) }
        setGameDetails (Some gameDetails)
        Html.h1 [
            Html.text "Starting new game..." ])
