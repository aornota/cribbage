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
    ForCribWorker : WorkerFunc<IsDealer * CardL, CardL>
    // TODO-NMB: Use a non-anonymous record (i.e. to avoid repeating this)?...
    PegWorker : WorkerFunc<{| previouslyPegged : Pegged list ; pegged : Pegged ; peggable : Card list ; notPeggable : Card list ; cutCard : Card ; selfCrib : Card list ; isDealer : IsDealer |}, bool * Card> }

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

type private GameDetails = {
    Player1 : PlayerDetails
    Player2 : PlayerDetails
    GameEngine : GameEngine }

let [<Literal>] private FOR_CRIB_WORKER_TIMEOUT = 10000
let [<Literal>] private PEG_WORKER_TIMEOUT = 5000

let private toAnon (pegState:PegState) = {|
    previouslyPegged = pegState.PreviouslyPegged
    pegged = pegState.Pegged
    peggable = pegState.Peggable |> List.ofSeq
    notPeggable = pegState.NotPeggable |> List.ofSeq
    cutCard = pegState.CutCard
    selfCrib = pegState.SelfCrib |> List.ofSeq
    isDealer = pegState.IsDealer |}

let private gameSummaries : cval<GameSummary list> = cval []

let private games' = React.functionComponent ("Games", fun (props:{| games : aval<int> ; isInteractive : bool |}) ->
    let games = ReactHB.Hooks.useAdaptive props.games
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
            Mui.avatar [
                avatar.variant.square
                prop.style [ style.verticalAlign.middle ; style.marginLeft (length.em 0.75) ]
                avatar.children [
                    Mui.icon [
                        if props.isInteractive then icon.color.primary else icon.color.secondary
                        icon.children [ if props.isInteractive then humanIcon [] else robotIcon [] ] ] ] ] ] ])
let private name name isInteractive = name' {| name = name ; isInteractive = isInteractive |}

let private forCribI' = React.functionComponent ("ForCribInteractive", fun (props:{| awaitingForCrib : aval<(IsDealer * Hand * (CardS -> unit)) option> |}) ->
    let awaitingForCrib = ReactHB.Hooks.useAdaptive props.awaitingForCrib
    // TODO-NMB: State for selected...
    match awaitingForCrib with
    | Some (isDealer, hand, _) ->
        // TODO-NMB: Allow toggle selected | allow confirm | ...
        Mui.typography [
            typography.variant.body1
            typography.color.textSecondary
            typography.align.center
            prop.style [ style.marginTop (length.em 0.75) ]
            typography.children [
                Html.strong "TODO-NMB: awaiting forCrib interactive"
                Html.text (sprintf " -> %b | %A..." isDealer hand) ] ]
    | None -> Html.none)
let private forCribI awaitingForCrib = forCribI' {| awaitingForCrib = awaitingForCrib |}

let private forCribNI' = React.functionComponent ("ForCribNonInteractive", fun (props:{| awaitingForCrib : aval<(IsDealer * Hand * (CardS -> unit)) option> ; forCribWorker : WorkerFunc<IsDealer * CardL, CardL> |}) ->
    let awaitingForCrib = ReactHB.Hooks.useAdaptive props.awaitingForCrib
    let worker, workerStatus = React.useWorker (props.forCribWorker, fun options -> { options with Timeout = Some FOR_CRIB_WORKER_TIMEOUT })
    // TODO-NMB: Np need for async - unless "monitoring" during development...
    let runWorker () = async {
        match awaitingForCrib with
        | Some (isDealer, hand, forCrib) when workerStatus <> WorkerStatus.Running && workerStatus <> WorkerStatus.Killed ->
            do! Async.Sleep 1000
            //do! Async.Sleep 10000
            worker.exec ((isDealer, hand |> List.ofSeq), Set.ofList >> forCrib)
        | Some _ -> Browser.Dom.console.log "Should never happen: forCribNI' runWorker () when Some awaitingForCrib but worker neither Running nor Killed"
        | None -> () }
    React.useEffect (runWorker >> Async.StartImmediate, [| box awaitingForCrib |])
    match awaitingForCrib with
    | Some (isDealer, hand, _) ->
        match workerStatus with
        | WorkerStatus.TimeoutExpired ->
            Mui.typography [
                typography.variant.body1
                typography.color.error
                prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.75) ]
                typography.children [ Html.text "Worker timeout expired" ] ]
        | WorkerStatus.Error error ->
            Mui.typography [
                typography.variant.body1
                typography.color.error
                prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.75) ]
                typography.children [ Html.text error ] ]
        | _ ->
            // TODO-NMB: Should show "hidden" cards (plus spinner)?...
            Html.div [
                prop.style [ style.display.flex ; style.justifyContent.center ; style.alignItems.center ; style.marginTop (length.em 0.75) ]
                prop.children [
                    yield! hand |> List.ofSeq |> List.map Card.render
                    (* Mui.typography [
                        typography.variant.body1
                        typography.color.textSecondary
                        typography.children [ Html.text (sprintf "Choosing cards for %s crib from %s..." (if isDealer then "own" else "opponent's") (cardsText hand)) ] ] *)
                    Mui.circularProgress [
                        circularProgress.variant.indeterminate
                        circularProgress.size (length.em 1.5)
                        circularProgress.color.secondary
                        prop.style [ style.marginLeft (length.em 0.75) ] ] ] ]
    | None -> Html.none)
let private forCribNI awaitingForCrib forCribWorker = forCribNI' {| awaitingForCrib = awaitingForCrib ; forCribWorker = forCribWorker |}

let private pegI' = React.functionComponent ("PegInteractive", fun (props:{| awaitingPeg : aval<(PegState * (Card option -> unit)) option> |}) ->
    let awaitingPeg = ReactHB.Hooks.useAdaptive props.awaitingPeg
    // TODO-NMB: State for selected...
    match awaitingPeg with
    | Some (pegState, _) ->
        // TODO-NMB: Allow toggle selected | allow confirm | ...
        Mui.typography [
            typography.variant.body1
            typography.color.textSecondary
            typography.align.center
            prop.style [ style.marginTop (length.em 0.75) ]
            typography.children [
                Html.strong "TODO-NMB: awaiting peg interactive"
                Html.text (sprintf " -> %A..." pegState) ] ]
    | None -> Html.none)
let private pegI awaitingPeg = pegI' {| awaitingPeg = awaitingPeg |}

let private pegNI' = React.functionComponent ("PegNonInteractive", fun (props:{| awaitingPeg : aval<(PegState * (Card option -> unit)) option> ; pegWorker : WorkerFunc<{| previouslyPegged : Pegged list ; pegged : Pegged ; peggable : Card list ; notPeggable : Card list ; cutCard : Card ; selfCrib : Card list ; isDealer : IsDealer |}, bool * Card> |}) ->
    let awaitingPeg = ReactHB.Hooks.useAdaptive props.awaitingPeg
    let worker, workerStatus = React.useWorker (props.pegWorker, fun options -> { options with Timeout = Some PEG_WORKER_TIMEOUT })
    // TODO-NMB: Np need for async - unless "monitoring" during development...
    let runWorker () = async {
        match awaitingPeg with
        | Some (pegState, peg) when workerStatus <> WorkerStatus.Running && workerStatus <> WorkerStatus.Killed ->
            do! Async.Sleep (match pegState.Peggable.Count with | 0 -> 500 | 1 -> 1250 | _ -> 2500)
            // Note: option<Card> also problematic - so hack around this.
            worker.exec (toAnon pegState, (fun (isSome, (rank, suit)) -> peg (if isSome then Some (rank, suit) else None)))
        | Some _ -> Browser.Dom.console.log "Should never happen: pegNI' runWorker () when Some awaitingPeg but worker neither Running nor Killed"
        | None -> () }
    React.useEffect (runWorker >> Async.StartImmediate, [| box awaitingPeg |])
    match awaitingPeg with
    | Some (pegState, _) ->
        match workerStatus with
        | WorkerStatus.TimeoutExpired ->
            Mui.typography [
                typography.variant.body1
                typography.color.error
                prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.75) ]
                typography.children [ Html.text "Worker timeout expired" ] ]
        | WorkerStatus.Error error ->
            Mui.typography [
                typography.variant.body1
                typography.color.error
                prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.75) ]
                typography.children [ Html.text error ] ]
        | _ ->
            // TODO-NMB: Should show "hidden" card/s (plus spinner)?...
            Html.div [
                prop.style [ style.display.flex ; style.justifyContent.center ; style.alignItems.center ; style.marginTop (length.em 0.75) ]
                prop.children [
                    let all = pegState.Peggable |> Set.union pegState.NotPeggable
                    if all.Count > 0 then yield! all |> List.ofSeq |> List.map Card.render
                    Mui.typography [
                        typography.variant.body1
                        typography.color.textSecondary
                        prop.style [ style.marginLeft (length.em 0.5) ]
                        typography.children [ Html.text (sprintf "(running total is %i)" (pips (pegState.Pegged |> List.map fst))) ] ]
                    (* Mui.typography [
                        typography.variant.body1
                        typography.color.textSecondary
                        typography.children [
                            if pegState.Peggable.Count > 0 then
                                Html.text (sprintf "Choosing card to peg from %s (running total is %i)..." (cardsText (pegState.Peggable |> Set.union pegState.NotPeggable)) (pips (pegState.Pegged |> List.map fst)))
                            else Html.text (sprintf "Claiming a go (running total is %i)..." (pips (pegState.Pegged |> List.map fst))) ] ] *)
                    Mui.circularProgress [
                        circularProgress.variant.indeterminate
                        circularProgress.size (length.em 1.5)
                        circularProgress.color.secondary
                        prop.style [ style.marginLeft (length.em 0.75) ] ] ] ]
    | None -> Html.none)
let private pegNI awaitingPeg pegWorker = pegNI' {| awaitingPeg = awaitingPeg ; pegWorker = pegWorker |}

// TODO-NMB: cannotPegI | newDeal[I|NI] | newGame[I|NI] | scoring (events) | ...

let private cannotPegNI' = React.functionComponent ("CannotPegNonInteractive", fun (props:{| awaitingCannotPeg : aval<(unit -> unit) option> |}) ->
    let awaitingCannotPeg = ReactHB.Hooks.useAdaptive props.awaitingCannotPeg
    let cannotPeg () = async {
        do! Async.Sleep 500
        match awaitingCannotPeg with | Some awaitingCannotPeg -> awaitingCannotPeg () | None -> () }
    React.useEffect (cannotPeg >> Async.StartImmediate, [| box awaitingCannotPeg |])
    match awaitingCannotPeg with
    | Some _ ->
        // TODO-NMB: What to display here?...
        Html.div [
            prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.75) ]
            prop.children [
                Mui.typography [
                    typography.variant.body1
                    typography.color.textSecondary
                    typography.children [ Html.text "Cannot peg..." ] ]
                Mui.circularProgress [
                    circularProgress.variant.indeterminate
                    circularProgress.size (length.em 1.5)
                    circularProgress.color.secondary
                    prop.style [ style.marginLeft (length.em 0.75) ] ] ] ]
    | None -> Html.none)
let private cannotPegNI awaitingCannotPeg = cannotPegNI' {| awaitingCannotPeg = awaitingCannotPeg |}

let private newDealNI' = React.functionComponent ("NewDealNonInteractive", fun (props:{| awaitingNewDeal : aval<(unit -> unit) option> |}) ->
    let awaitingNewDeal = ReactHB.Hooks.useAdaptive props.awaitingNewDeal
    let newDeal () = match awaitingNewDeal with | Some awaitingNewDeal -> awaitingNewDeal () | None -> ()
    React.useEffect (newDeal, [| box awaitingNewDeal |])
    Html.none)
let private newDealNI awaitingNewDeal = newDealNI' {| awaitingNewDeal = awaitingNewDeal |}

let private newGameNI' = React.functionComponent ("NewGameNonInteractive", fun (props:{| awaitingNewGame : aval<(unit -> unit) option> |}) ->
    let awaitingNewGame = ReactHB.Hooks.useAdaptive props.awaitingNewGame
    // TODO-NMB: No need for async/sleep [only used for monitoring during development]...
    let newGame () = async {
        do! Async.Sleep 2500
        match awaitingNewGame with | Some awaitingNewGame -> awaitingNewGame () | None -> () }
    React.useEffect (newGame >> Async.StartImmediate, [| box awaitingNewGame |])
    Html.none)
let private newGameNI awaitingNewGame = newGameNI' {| awaitingNewGame = awaitingNewGame |}

let private score' = React.functionComponent ("Score", fun (props:{| score : aval<int> ; isInteractive : bool |}) ->
    let score = ReactHB.Hooks.useAdaptive props.score
    let progress1Max, progress2Max = 61, 121
    let progress1 = if score >= progress1Max then 100 else score * 100 / progress1Max
    let progress2 = if score <= progress1Max then 0 else min ((score - progress1Max) * 100 / (progress2Max - progress1Max)) 100
    React.fragment [
        Mui.typography [
            typography.variant.h3
            if score = 0 then typography.color.textSecondary else if props.isInteractive then typography.color.primary else typography.color.secondary
            prop.style [ style.display.flex ; style.justifyContent.center ]
            typography.children [ Html.strong score ] ]
        Mui.linearProgress [
            linearProgress.variant.determinate
            if props.isInteractive then linearProgress.color.primary else linearProgress.color.secondary
            linearProgress.value progress1
            prop.style [ style.height (length.em 0.33) ; style.marginBottom (length.em 0.33) ] ]
        Mui.linearProgress [
            linearProgress.variant.determinate
            if props.isInteractive then linearProgress.color.primary else linearProgress.color.secondary
            linearProgress.value progress2
            prop.style [ style.height (length.em 0.33) ; style.marginBottom (length.em 0.75) ] ]
    ])
let private score score isInteractive = score' {| score = score ; isInteractive = isInteractive |}

// TODO-NMB: Implement events - including clearing when deal changes (and auto-expanding behaviour depending on "what's changed"?)...
let private isDealerAndScoreEvents' = React.functionComponent ("IsDealerAndScoreEvents", fun (props:{| isDealer : aval<int option> ; isInteractive : bool |}) ->
    let isDealer = ReactHB.Hooks.useAdaptive props.isDealer
    match isDealer with
    | Some deal ->
        Html.div [
            prop.children [
                Html.div [
                    prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.5) ]
                    prop.children [
                        Mui.chip [
                            if props.isInteractive then chip.color.primary else chip.color.secondary
                            chip.label (sprintf "Dealer for hand #%i" deal) ] ] ]
                (* TEMP-NMB...
                Html.div [
                    prop.style [ style.display.flex ; style.justifyContent.center ; style.marginTop (length.em 0.5) ]
                    prop.children [
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
                                                        Mui.listItemText [ listItemText.primary "2 for his nibs (Jc)" ] ] ] ] ] ] ] ] ] ] ] *)
        ] ]
    | None -> Html.none)
let private isDealerAndScoreEvents isDealer isInteractive = isDealerAndScoreEvents' {| isDealer = isDealer ; isInteractive = isInteractive |}

let private player' = React.memo ("Player", fun (props:{| player : PlayerDetails |}) ->
    let isInteractive, forCrib, peg, cannotPeg, newDeal, newGame =
        match props.player.PlayerType with
        | Interactive ->
            true,
            forCribI props.player.PlayerHooks.AwaitingForCrib,
            pegI props.player.PlayerHooks.AwaitingPeg,
            Html.none, // TODO-NMB...cannotPegI props.player.PlayerHooks.AwaitingCannotPeg
            Html.none, // TODO-NMB...cannotPegI props.player.PlayerHooks.AwaitingNewDeal
            Html.none // TODO-NMB...cannotPegI props.player.PlayerHooks.AwaitingNewGame
        | NonInteractive workers ->
            false,
            forCribNI props.player.PlayerHooks.AwaitingForCrib workers.ForCribWorker,
            pegNI props.player.PlayerHooks.AwaitingPeg workers.PegWorker,
            cannotPegNI props.player.PlayerHooks.AwaitingCannotPeg,
            newDealNI props.player.PlayerHooks.AwaitingNewDeal,
            newGameNI props.player.PlayerHooks.AwaitingNewGame
    Mui.card [
        prop.style [ style.minHeight (length.em 16) ; style.paddingLeft (length.em 0.75) ; style.paddingRight (length.em 0.75) ; style.paddingBottom (length.em 0.75) ]
        card.children [
            Mui.grid [
                grid.container true
                grid.spacing._2
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
                            forCrib
                            // TODO-NMB: Pegging hand - if not own turn (and also before pegging starts, e.g. if waiting for opponent forCrib)...
                            peg
                            cannotPeg
                            // TODO-NMB: Hand (once pegging completed) - and crib (if appropriate)...
                            newDeal
                            newGame ] ]
                    Mui.grid [
                        grid.xs._3
                        grid.item true
                        grid.children [
                            score props.player.PlayerHooks.Score isInteractive
                            isDealerAndScoreEvents props.player.PlayerHooks.IsDealer isInteractive ] ] ] ] ] ])
let private player player = player' {| player = player |}

// TODO-NMB: "Shared" area (deck | crib | pegging | &c.)...

let private game' = React.memo ("Game", fun (props:{| showToast : Toaster.ToastData -> unit |}) ->
    let gameDetails, setGameDetails : GameDetails option * (GameDetails option -> unit) = React.useState None
    (* IMPORTANT NOTE: Do *not* call "Theme.useStyles ()" as this causes a re-render (despite memo-ization) when light/dark theme changed - which seems to screw up state / adaptive stuff. *)
    match gameDetails with
    | Some gameDetails ->
        React.fragment [
            player gameDetails.Player1
            // TEMP-NMB...
            Html.div [ prop.style [ style.height (length.em 8) ] ]
            // ...TEMP-NMB
            player gameDetails.Player2
            // TODO-NMB: Statistics?...
        ]
    | None ->
        // TODO-NMB: UI to enter name | select opponent skill level | &c.?...
        //let name1, name2 = "Bender", "Neph"
        let name1, name2 = "Bender", "Marvin"
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
            PlayerType = NonInteractive { ForCribWorker = Strategies.forCribIntermediateWorker ; PegWorker = Strategies.pegIntermediateWorker }
            PlayerHooks = hooks Player1 }
        let player2 = {
            Name = name2
            //PlayerType = Interactive
            PlayerType = NonInteractive { ForCribWorker = Strategies.forCribBasicWorker ; PegWorker = Strategies.pegBasicWorker }
            //PlayerType = NonInteractive { ForCribWorker = Strategies.forCribRandomWorker ; PegWorker = Strategies.pegRandomWorker }
            PlayerHooks = hooks Player2 }
        let gameDetails = { Player1 = player1 ; Player2 = player2 ; GameEngine = engine }
        engine.GameOverEvent.Add (fun summary ->
            // TODO-NMB: Finesse toast - plus update statistics?...
            transact (fun _ -> gameSummaries.Value <- summary :: gameSummaries.Value)
            (* TEMP-NMB... *)
            let winner = if summary.IsWinner(Player1) then Player1 else Player2
            let isInteractive = function | Interactive -> true | NonInteractive _ -> false
            let toast : Toaster.ToastData = {
                Title = Some "Game over, man. Game over."
                Icon = Some (if isInteractive (if winner = Player1 then player1.PlayerType else player2.PlayerType) then Toaster.Human else Toaster.Computer)
                Message = sprintf "%s wins game #%i." (if winner = Player1 then player1.Name else player2.Name) gameSummaries.Value.Length
                Purpose = Toaster.Information
                Affinity = None
                TimeoutOverride = None }
            props.showToast toast
            )
        (* async {
            do! Async.Sleep 1000
            setGameDetails (Some gameDetails) } |> Async.StartImmediate
        Mui.dialog [
            dialog.disableBackdropClick true
            dialog.disableEscapeKeyDown true
            dialog.open' true
            prop.style [ style.userSelect.none ]
            dialog.children [
                Mui.dialogTitle "Please wait..."
                Mui.dialogContent [
                    prop.style [ style.display.flex ; style.justifyContent.center ; style.marginBottom (length.em 1) ]
                    dialogContent.children [
                        Mui.circularProgress [
                            circularProgress.variant.indeterminate
                            circularProgress.color.inherit' ] ] ] ] ] *)
        setGameDetails (Some gameDetails)
        Html.none)
let game showToast = game' {| showToast = showToast |}
