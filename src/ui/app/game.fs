[<RequireQualifiedAccess>]
module Aornota.Cribbage.Ui.Game

open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.GameEngine
open Aornota.Cribbage.Domain.Scoring
open Aornota.Cribbage.Domain.Strategy

open Aornota.Cribbage.Ui.Workers

open Fable.React.Adaptive
module ReactHB = Fable.React.HookBindings

open Feliz
open Feliz.MaterialUI
open Fable.MaterialUI.MaterialDesignIcons
open Feliz.UseWorker

open FSharp.Data.Adaptive

type private StrategyWorkers = {
    ForCrib : WorkerFunc<IsDealer * CardL, CardL>
    Peg : WorkerFunc<{| previouslyPegged : Pegged list ; pegged : Pegged ; peggable : Card list ; notPeggable : Card list ; cutCard : Card ; selfCrib : Card list ; isDealer : IsDealer |}, bool * Card> }

type private PlayerType = | Interactive | NonInteractive of StrategyWorkers

type private PlayerHooks = {
    Games : aval<int>
    Score : aval<int>
    IsDealer : aval<int option>
    // Hand?...
    AwaitingForCrib : aval<(IsDealer * Hand * (CardS -> unit)) option>
    // PeggingHand?...
    AwaitingPeg : aval<(PegState * (Card option -> unit)) option>
    AwaitingCannotPeg : aval<(unit -> unit) option>
    AwaitingNewDeal : aval<(unit -> unit) option>
    AwaitingNewGame : aval<(unit -> unit) option> }

type private PlayerDetails = {
    Name : string
    PlayerType : PlayerType
    PlayerHooks : PlayerHooks }

type private GameDetails = { // TODO-NMB: More, e.g. game summaries (cf. game-player.fs) - or should these be cval?...
    Player1 : PlayerDetails
    Player2 : PlayerDetails
    GameEngine : GameEngine }

let private gameSummaries : cval<GameSummary list> = cval []

// TODO-NMB: See game-TEMP.fs for adaptive | web worker stuff...

let private games' = React.functionComponent ("Games", fun (props:{| games : aval<int> ; isInteractive : bool |}) ->
    let games = ReactHB.Hooks.useAdaptive props.games
    // TEMP-NMB...
    //let games = if props.isInteractive then 2 else 1
    // ...TEMP-NMB
    Mui.typography [
        typography.variant.h3
        if games = 0 then typography.color.textSecondary else if props.isInteractive then typography.color.primary else typography.color.secondary
        typography.children [ Html.strong games ] ])
let private games games isInteractive = games' {| games = games ; isInteractive = isInteractive |}

let private name' = React.memo ("Name", fun (props:{| name : string ; isInteractive : bool |}) ->
    Mui.typography [
        typography.variant.h3
        prop.style [ style.display.flex ; style.alignItems.center ; style.justifyContent.center ]
        typography.children [
            Html.strong props.name
            Mui.icon [
                if props.isInteractive then icon.color.primary else icon.color.secondary
                prop.style [ style.verticalAlign.middle ; style.marginLeft (length.em 0.5) ]
                icon.children [ if props.isInteractive then humanIcon [] else robotIcon [] ] ] ] ])
let private name name isInteractive = name' {| name = name ; isInteractive = isInteractive |}

let private isDealer' = React.functionComponent ("IsDealer", fun (props:{| isDealer : aval<int option> ; isInteractive : bool |}) ->
    let isDealer = ReactHB.Hooks.useAdaptive props.isDealer
    match isDealer with
    | Some deal ->
        Html.div [
            prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.5) ]
            prop.children [
                Mui.chip [
                    if props.isInteractive then chip.color.primary else chip.color.secondary
                    chip.label (sprintf "Dealer for hand #%i" deal) ] ] ]
    | None -> Html.none)
let private isDealer isDealer isInteractive = isDealer' {| isDealer = isDealer ; isInteractive = isInteractive |}

let private score' = React.functionComponent ("Score", fun (props:{| score : aval<int> ; isInteractive : bool |}) ->
    let score = ReactHB.Hooks.useAdaptive props.score
    // TEMP-NMB...
    //let score = if props.isInteractive then 77 else 83
    // ...TEMP-NMB
    let progress = min (score * 100 / 121) 100
    React.fragment [
        Mui.typography [
            typography.variant.h3
            if score = 0 then typography.color.textSecondary else if props.isInteractive then typography.color.primary else typography.color.secondary
            prop.style [ style.display.flex ; style.justifyContent.center ]
            typography.children [ Html.strong score ] ]
        Mui.linearProgress [
            linearProgress.variant.determinate
            if props.isInteractive then linearProgress.color.primary else linearProgress.color.secondary
            linearProgress.value progress
            prop.style [ style.height (length.em 0.5) ; style.marginBottom (length.em 0.75) ] ] ])
let private score score isInteractive = score' {| score = score ; isInteractive = isInteractive |}

// TODO-NMB: Accordion/s for events...

let private player' = React.functionComponent ("Player", fun (props:{| player : PlayerDetails |}) ->
    let isInteractive = match props.player.PlayerType with | Interactive -> true | NonInteractive _ -> false
    Mui.card [
        prop.style [ style.paddingLeft (length.em 0.75) ; style.paddingRight (length.em 0.75) ; style.paddingBottom (length.em 0.75) ]
        card.children [
            Mui.grid [
                grid.container true
                //?grid.spacing._2
                grid.children [
                    Mui.grid [
                        grid.xs._1
                        grid.item true
                        grid.children [ games props.player.PlayerHooks.Games isInteractive ] ]
                    Mui.grid [
                        grid.xs._8
                        grid.item true
                        grid.children [
                            name props.player.Name isInteractive
                            isDealer props.player.PlayerHooks.IsDealer isInteractive
                            // TEMP-NMB...
                            Mui.typography [
                                typography.variant.h6
                                typography.color.textSecondary
                                typography.align.center
                                prop.style [ style.marginTop (length.em 0.75) ]
                                typography.children [ Html.em "TODO-NMB..." ] ]
                            // ...TEMP-NMB
                            ] ]
                    Mui.grid [
                        grid.xs._3
                        grid.item true
                        grid.children [
                            score props.player.PlayerHooks.Score isInteractive
                            // TEMP-NMB...
                            Mui.accordion [
                                //?accordion.defaultExpanded true
                                accordion.children [
                                    Mui.accordionSummary [
                                        accordionSummary.disabled true
                                        accordionSummary.expandIcon (chevronDownIcon [])
                                        accordionSummary.children [
                                            Mui.typography [ Html.text "Cut events" ] ] ]
                                    Mui.accordionDetails [
                                        accordionDetails.children [
                                            Mui.list [
                                                list.dense true
                                                list.disablePadding true
                                                list.children [
                                                    Mui.listItem [
                                                        listItem.children [
                                                            Mui.listItemText [ listItemText.primary "2 for his nibs (Jc)" ] ] ] ] ] ] ] ] ]
                            // ...TEMP-NMB
                        ] ] ] ] ] ])
let private player player = player' {| player = player |}

// TODO-NMB: "Shared" area (deck | crib | pegging | &c.)...

let private game' = React.memo ("Game", fun (props:{| showToast : Toaster.ToastData -> unit |}) ->
    let gameDetails, setGameDetails : GameDetails option * (GameDetails option -> unit) = React.useState (None)
    (* IMPORTANT NOTE: Do *not* call "Theme.useStyles ()" as this causes a re-render (despite memo-ization) when light/dark theme changed - which seems to screw up state / adaptive stuff. *)
    match gameDetails with
    | Some gameDetails ->
        React.fragment [
            player gameDetails.Player1
            // TEMP-NMB...
            Html.div [ prop.style [ style.height (length.em 6) ] ]
            // ...TEMP-NMB
            player gameDetails.Player2
            // TODO-NMB: Statistics?...
        ]
    | None ->
        // TODO-NMB: UI to enter name | select opponent skill level | &c. | ...
        let name1, name2 = "Bender", "Neph"
        //let name1, name2 = "Bender", "Marvin"
        let engine = GameEngine (name1, name2)
        let hooks player = {
            Games = gameSummaries |> AVal.map (fun summaries -> summaries |> List.sumBy (fun summary -> if summary.IsWinner(player) then 1 else 0))
            Score = engine.Scores |> AVal.map (fun (score1, score2) -> int (if player = Player1 then score1 else score2))
            IsDealer = engine.IsDealer(player)
            AwaitingForCrib = engine.AwaitingForCrib(player)
            AwaitingPeg = engine.AwaitingPeg(player)
            AwaitingCannotPeg = engine.AwaitingCannotPeg(player)
            AwaitingNewDeal = engine.AwaitingNewDeal(player)
            AwaitingNewGame = engine.AwaitingNewGame(player) }
        let player1 = {
            Name = name1
            PlayerType = NonInteractive { ForCrib = Strategies.forCribBasicWorker ; Peg = Strategies.pegBasicWorker }
            PlayerHooks = hooks Player1 }
        let player2 = {
            Name = name2
            PlayerType = Interactive
            //PlayerType = NonInteractive { ForCrib = Strategies.forCribIntermediateWorker ; Peg = Strategies.pegIntermediateWorker }
            PlayerHooks = hooks Player2 }
        let gameDetails = { Player1 = player1 ; Player2 = player2 ; GameEngine = engine }
        async {
            do! Async.Sleep 2500
            setGameDetails (Some gameDetails) } |> Async.StartImmediate
        Html.div [
            Mui.dialog [
                dialog.disableBackdropClick true
                dialog.disableEscapeKeyDown true
                dialog.open' true
                prop.style [ style.userSelect.none ]
                dialog.children [
                    Mui.dialogTitle "Please wait"
                    Mui.dialogContent [
                        prop.style [ style.display.flex ; style.justifyContent.center ; style.marginBottom (length.em 1) ]
                        dialogContent.children [
                            Mui.circularProgress [
                                circularProgress.variant.indeterminate
                                circularProgress.color.inherit' ] ] ] ] ] ])
let game showToast = game' {| showToast = showToast |}
