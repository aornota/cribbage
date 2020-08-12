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

type private PlayerDetails = { // TODO-NMB: Distinguish between non-interactive (with forCrib | peg strategies) and interactive?...
    Name : string
    ForCribStrategyWorker : WorkerFunc<IsDealer * CardL, CardL>
    PegStrategyWorker :
        WorkerFunc<{| previouslyPegged : Pegged list ; pegged : Pegged ; peggable : Card list ; notPeggable : Card list ; cutCard : Card ; selfCrib : Card list ; isDealer : IsDealer |}, bool * Card>}

type private GameDetails = { // TODO-NMB: More, e.g. game summaries (cf. game-player.fs)?...
    Player1Details : PlayerDetails
    Player2Details : PlayerDetails
    GameEngine : GameEngine }

// TODO-NMB: Components...

let private game' = React.memo ("Game", fun (props:{| showToast : Toaster.ToastData -> unit |}) ->
    let gameDetails, setGameDetails : GameDetails option * (GameDetails option -> unit) = React.useState (None)
    (* IMPORTANT NOTE: Do *not* call "Theme.useStyles ()" as this causes a re-render (despite memo-ization) when light/dark theme changed - which seems to screw up state / adaptive stuff. *)
    match gameDetails with
    | Some gameDetails ->
        // TODO-NMB: See game-TEMP.fs for adaptive | web worker stuff...
        // TODO-NMB: "Scaffold" UI...
        Html.div [
            Mui.card [
                prop.children [
                    Mui.grid [
                        grid.container true
                        prop.children [
                            Mui.grid [
                                grid.xs._9
                                grid.item true
                                prop.style [ style.padding (length.em 0.75)]
                                prop.children [
                                    Mui.typography [
                                        typography.variant.h4
                                        typography.align.center
                                        prop.style [ style.marginBottom (length.em 0.25) ]
                                        prop.children [
                                            Html.strong "Bender"
                                            Mui.badge [
                                                badge.color.primary
                                                badge.badgeContent 2
                                                badge.showZero true
                                                prop.style [ style.marginLeft (length.em 0.25) ]
                                                prop.children [ robotIcon [] ] ]
                                            if true then // i.e. only if dealer
                                                Html.br []
                                                Mui.chip [
                                                    chip.color.secondary
                                                    prop.style [ style.marginLeft (length.em 1) ]
                                                    chip.label "Dealer for hand #7" ]
                                    ] ]
                                    Mui.divider []
                                    // More, e.g. forCrib shizzle...
                            ] ]
                            Mui.grid [
                                grid.xs._3
                                grid.item true
                                prop.style [ style.padding (length.em 0.75) ]
                                prop.children [
                                    Mui.typography [
                                        typography.variant.h2
                                        typography.align.center
                                        prop.children [ Html.strong "93" ] ]
                                    Mui.linearProgress [
                                        linearProgress.variant.determinate
                                        linearProgress.color.primary
                                        linearProgress.value 77 // i.e. (93 / 121) * 100
                                        prop.style [
                                            style.height (length.em 0.75)
                                            style.marginBottom (length.em 0.75) ] ]
                                    Mui.accordion [
                                        prop.children [
                                            Mui.accordionSummary [
                                                accordionSummary.expandIcon (chevronDownIcon [])
                                                prop.children [
                                                    Mui.typography [ Html.text "Cut events" ] ] ]
                                            Mui.accordionDetails [
                                                prop.children [
                                                    Mui.list [
                                                        list.dense true
                                                        list.disablePadding true
                                                        prop.children [
                                                            Mui.listItem [
                                                                prop.children [
                                                                    Mui.listItemText [ listItemText.primary "2 for his nibs (Jc)" ] ] ] ] ] ] ] ] ]
                                    Mui.accordion [
                                        //accordion.defaultExpanded true
                                        prop.children [
                                            Mui.accordionSummary [
                                                accordionSummary.expandIcon (chevronDownIcon [])
                                                prop.children [
                                                    Mui.typography [ Html.strong "Pegging events" ] ] ]
                                            Mui.accordionDetails [
                                                prop.children [
                                                    Mui.list [
                                                        list.dense true
                                                        list.disablePadding true
                                                        prop.children [
                                                            Mui.listItem [
                                                                prop.children [
                                                                    Mui.listItemText [ listItemText.primary "15 for 2" ] ] ]
                                                            Mui.listItem [
                                                                prop.children [
                                                                    Mui.listItemText [ listItemText.primary "29 for 2" ] ] ]
                                                            Mui.listItem [
                                                                prop.children [
                                                                    Mui.listItemText [ listItemText.secondary "Go for 1" ] ] ] ] ] ] ] ] ]
                            ] ]
            ] ] ] ]

            Html.div [ prop.style [ style.height (length.em 6) ] ]

            Mui.card [
                prop.children [
                    Mui.grid [
                        grid.container true
                        prop.children [
                            Mui.grid [
                                grid.xs._9
                                grid.item true
                                prop.style [ style.padding (length.em 0.75)]
                                prop.children [
                                    Mui.typography [
                                        typography.variant.h4
                                        typography.align.center
                                        prop.style [ style.marginBottom (length.em 0.25) ]
                                        prop.children [
                                            Html.strong "Marvin"
                                            Mui.badge [
                                                badge.color.primary
                                                badge.badgeContent 1
                                                badge.showZero true
                                                prop.style [ style.marginLeft (length.em 0.25) ]
                                                prop.children [ robotIcon [] ] ] ] ]
                                    Mui.divider []
                                    // More, e.g. forCrib shizzle...
                            ] ]
                            Mui.grid [
                                grid.xs._3
                                grid.item true
                                prop.style [ style.padding (length.em 0.75) ]
                                prop.children [
                                    Mui.typography [
                                        typography.variant.h2
                                        typography.align.center
                                        prop.children [ Html.strong "78" ] ]
                                    Mui.linearProgress [
                                        linearProgress.variant.determinate
                                        linearProgress.color.primary
                                        linearProgress.value 64 // i.e. (78 / 121) * 100
                                        prop.style [
                                            style.height (length.em 0.75)
                                            style.marginBottom (length.em 0.75) ] ]
                                    Mui.accordion [
                                        //accordion.defaultExpanded true
                                        prop.children [
                                            Mui.accordionSummary [
                                                accordionSummary.disabled true
                                                accordionSummary.expandIcon (chevronDownIcon [])
                                                prop.children [
                                                    Mui.typography [ Html.strong "Pegging events" ] ] ] ] ]
                            ] ]
            ] ] ] ]

        ]
    | None ->
        // TODO-NMB: UI to enter name | select opponent skill level | &c. | ...
        let player1Details : PlayerDetails = { Name = "Basic" ; ForCribStrategyWorker = Strategies.forCribBasicWorker ; PegStrategyWorker = Strategies.pegBasicWorker }
        let player2Details : PlayerDetails = { Name = "Neph" ; ForCribStrategyWorker = Strategies.forCribIntermediateWorker ; PegStrategyWorker = Strategies.pegIntermediateWorker }
        let gameDetails = {
            Player1Details = player1Details
            Player2Details = player2Details
            GameEngine = GameEngine (player1Details.Name, player2Details.Name) }
        setGameDetails (Some gameDetails)
        Html.h1 [
            Html.text "Starting new game..." ])
let game showToast = game' {| showToast = showToast |}
