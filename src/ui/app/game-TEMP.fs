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

let [<Literal>] private FOR_CRIB_WORKER_TIMEOUT = 10000
let [<Literal>] private PEG_WORKER_TIMEOUT = 5000

let [<Literal>] private SLEEP = 250

let private toAnon (pegState:PegState) = {|
    previouslyPegged = pegState.PreviouslyPegged
    pegged = pegState.Pegged
    peggable = pegState.Peggable |> List.ofSeq
    notPeggable = pegState.NotPeggable |> List.ofSeq
    cutCard = pegState.CutCard
    selfCrib = pegState.SelfCrib |> List.ofSeq
    isDealer = pegState.IsDealer |}

// TODO-NMB: Should scores | awaitingForCrib | &c. be React.memo?...

let private scores' = React.functionComponent ("Scores", fun (props:{| scores : aval<int<point> * int<point>> ; player1 : PlayerDetails ; player2 : PlayerDetails |}) ->
    let player1Score, player2Score = ReactHB.Hooks.useAdaptive props.scores
    Mui.typography [
        typography.paragraph true
        typography.children [
            Html.strong props.player1.Name
            Html.text (sprintf " %i - %i "player1Score player2Score)
            Html.strong props.player2.Name ] ])
let private scores (scores, player1, player2) = scores' {| scores = scores ; player1 = player1 ; player2 = player2 |}

let private crib' = React.functionComponent ("Crib", fun (props:{| crib : aval<CardS option> |}) ->
    let crib = ReactHB.Hooks.useAdaptive props.crib
    match crib with
    | Some cards ->
        Mui.typography [
            typography.children [
                Html.text (sprintf "Crib -> %A" (cardsText cards)) ] ]
    | None -> Html.none)
let private crib crib = crib' {| crib = crib |}

let private cutCard' = React.functionComponent ("CutCard", fun (props:{| cutCard : aval<Card option> |}) ->
    let cutCard = ReactHB.Hooks.useAdaptive props.cutCard
    match cutCard with
    | Some card ->
        Mui.typography [
            typography.children [
                Html.text (sprintf "Cut card -> %A" (cardText card)) ] ]
    | None -> Html.none)
let private cutCard cutCard = cutCard' {| cutCard = cutCard |}

// TODO-NMB: Rethink naming (fForCrib &c.)?...

let private awaitingForCrib' = React.functionComponent ("AwaitingForCrib", fun (props:{| fForCrib : aval<(IsDealer * Hand * (CardS -> unit)) option> ; player : PlayerDetails |}) ->
    let fForCrib = ReactHB.Hooks.useAdaptive props.fForCrib
    let worker, workerStatus = React.useWorker (props.player.ForCribStrategyWorker, fun options -> { options with Timeout = Some FOR_CRIB_WORKER_TIMEOUT })
    let runWorker () =
        match fForCrib with
        | Some (isDealer, hand, forCrib) when workerStatus <> WorkerStatus.Running && workerStatus <> WorkerStatus.Killed -> worker.exec ((isDealer, hand |> List.ofSeq), Set.ofList >> forCrib)
        | Some _ -> Browser.Dom.console.log "Should never happen: awaitingPForCrib' runWoeker when Some fForCrib but worker neither Running nor Killed"
        | None -> ()
    React.useEffect (runWorker, [| box fForCrib |])
    match fForCrib with
    | Some (isDealer, hand, _) ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingForCrib (%s)" props.player.Name)
                Html.text (sprintf " -> %b | %A..." isDealer hand)
                Html.strong (sprintf "%A" workerStatus) ] ]
    | None -> Html.none)
let private awaitingForCrib (fForCrib, player) = awaitingForCrib' {| fForCrib = fForCrib ; player = player |}

// TODO-NMB: Why is this React.memo rather than React.functionComponent?...
let private awaitingPeg' = React.memo ("AwaitingPeg", fun (props:{| fPeg : aval<(PegState * (Card option -> unit)) option> ; player : PlayerDetails |}) ->
    let fPeg = ReactHB.Hooks.useAdaptive props.fPeg
    let worker, workerStatus = React.useWorker (props.player.PegStrategyWorker, fun options -> { options with Timeout = Some PEG_WORKER_TIMEOUT })
    let runWorker () =
        match fPeg with
        | Some (pegState, peg) when workerStatus <> WorkerStatus.Running && workerStatus <> WorkerStatus.Killed ->
            // Note: option<Card> also problematic - so hack around this.
            worker.exec (toAnon pegState, (fun (isSome, (rank, suit)) -> peg (if isSome then Some (rank, suit) else None)))
        | Some _ -> Browser.Dom.console.log "Should never happen: awaitingPeg' runWoeker when Some fpeg but worker neither Running nor Killed"
        | None -> ()
    React.useEffect (runWorker, [| box fPeg |])
    match fPeg with
    | Some (pegState, _) ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingPeg (%s)" props.player.Name)
                Html.text (sprintf " -> %A..." pegState)
                Html.strong (sprintf "%A" workerStatus) ] ]
    | None -> Html.none)
let private awaitingPeg (fPeg, player) = awaitingPeg' {| fPeg = fPeg ; player = player |}

let private awaitingCannotPeg' = React.functionComponent ("AwaitingCannotPeg", fun (props:{| fCannotPeg : aval<(unit -> unit) option> ; player : PlayerDetails |}) ->
    let fCannotPeg = ReactHB.Hooks.useAdaptive props.fCannotPeg
    let cannotPeg () = async {
        do! Async.Sleep SLEEP
        match fCannotPeg with | Some cannotPeg -> cannotPeg () | None -> () }
    React.useEffect (cannotPeg >> Async.StartImmediate, [| box fCannotPeg |])
    match fCannotPeg with
    | Some _ ->
        Mui.typography [
            typography.children [
                Html.em (sprintf "awaitingCannotPeg (%s)..." props.player.Name) ] ]
    | None -> Html.none)
let private awaitingCannotPeg (fCannotPeg, player) = awaitingCannotPeg' {| fCannotPeg = fCannotPeg ; player = player |}

let private awaitingNewDeal' = React.functionComponent ("AwaitingNewDeal", fun (props:{| fNewDeal : aval<(unit -> unit) option> ; player : PlayerDetails |}) ->
    let fNewDeal = ReactHB.Hooks.useAdaptive props.fNewDeal
    let newDeal () = async {
        do! Async.Sleep SLEEP
        match fNewDeal with | Some newDeal -> newDeal () | None -> () }
    React.useEffect (newDeal >> Async.StartImmediate, [| box fNewDeal |])
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

let private dealer' = React.functionComponent ("Dealer", fun (props:{| dealer : aval<Player> ; player1 : PlayerDetails ; player2 : PlayerDetails ; showToast : Toaster.ToastData -> unit |}) ->
    let dealer = ReactHB.Hooks.useAdaptive props.dealer
    let showToast () =
        let name = function | Player1 -> props.player1.Name | Player2 -> props.player2.Name
        let icon = function | Player1 -> Toaster.Computer | Player2 -> Toaster.Human // TODO-NMB: Should obtain from PlayerDetails...
        let data : Toaster.ToastData = {
            Title = Some "New deal"
            Icon = Some (icon dealer)
            Message = sprintf "%s is the dealer" (name dealer)
            Purpose = Toaster.Success
            Affinity = None
            TimeoutOverride = None }
        data |> props.showToast
    React.useEffect (showToast, [| box dealer |])
    Html.none)
let private dealer dealer player1 player2 showToast = dealer' {| dealer = dealer ; player1 = player1 ; player2 = player2 ; showToast = showToast |}

let private nibsScoreEvent' = React.functionComponent ("NibsScoreEvent", fun (props:{| e : IEvent<Player * Card * NibsScoreEvent> ; player1 : PlayerDetails ; player2 : PlayerDetails ; showToast : Toaster.ToastData -> unit |}) ->
    let subscribe () =
        let handler (player, cutCard, event:NibsScoreEvent) =
            let name = function | Player1 -> props.player1.Name | Player2 -> props.player2.Name
            let data : Toaster.ToastData = {
                Title = Some "Cut"
                Icon = Some Toaster.Score
                Message = sprintf "%s -> %s scores %s" (cardText cutCard) (name player) event.Text
                Purpose = Toaster.Information
                Affinity = Some player
                TimeoutOverride = None }
            data |> props.showToast
        props.e.Add handler
    React.useEffect (subscribe, [| |])
    Html.none)
let private nibsScoreEvent e player1 player2 showToast = nibsScoreEvent' {| e = e ; player1 = player1 ; player2 = player2 ; showToast = showToast |}

let private peggingScoreEvent' = React.functionComponent ("PeggingScoreEvent", fun (props:{| e : IEvent<Player * option<Card> * CardL * list<PeggingScoreEvent>> ; player1 : PlayerDetails ; player2 : PlayerDetails ; showToast : Toaster.ToastData -> unit |}) ->
    let subscribe () =
        let handler (player, card, pegged, events:PeggingScoreEvent list) =
            let name = function | Player1 -> props.player1.Name | Player2 -> props.player2.Name
            let played = match card with | Some card -> Some (cardText card, (fst card).PipValue) | None -> None
            let score = events |> List.sumBy (fun event -> event.Score)
            let message =
                match played with
                | Some (played, pipValue)  ->
                    let runningTotal = pips pegged + pipValue
                    let previous = match pegged |> List.map cardText with | h :: t -> sprintf "(%s) " (h :: t |> String.concat " ") | [] -> ""
                    sprintf "%s plays %s%s = %i -> scores %i" (name player) previous played runningTotal score
                | None -> sprintf "%s claims a go -> scores %i" (name player) score
            // TODO-NMB: More details, e.g. each event.Text (cf. game-player.fs)?...
            let data : Toaster.ToastData = {
                Title = Some "Pegging"
                Icon = Some Toaster.Score
                Message = message
                Purpose = Toaster.Information
                Affinity = Some player
                TimeoutOverride = None }
            data |> props.showToast
        props.e.Add handler
    React.useEffect (subscribe, [| |])
    Html.none)
let private peggingScoreEvent e player1 player2 showToast = peggingScoreEvent' {| e = e ; player1 = player1 ; player2 = player2 ; showToast = showToast |}

let private handScoreEvent' = React.functionComponent ("HandScoreEvent", fun (props:{| e : IEvent<Player * Hand * Card * list<HandScoreEvent>> ; player1 : PlayerDetails ; player2 : PlayerDetails ; showToast : Toaster.ToastData -> unit |}) ->
    let subscribe () =
        let handler (player, hand, card, events:HandScoreEvent list) =
            let name = function | Player1 -> props.player1.Name | Player2 -> props.player2.Name
            let score = events |> List.sumBy (fun event -> event.Score)
            // TODO-NMB: More details, e.g. each event.Text (cf. game-player.fs)?...
            let data : Toaster.ToastData = {
                Title = Some "Hand"
                Icon = Some Toaster.Score
                Message = sprintf "%s scores %i" (name player) score
                Purpose = Toaster.Information
                Affinity = Some player
                TimeoutOverride = None }
            data |> props.showToast
        props.e.Add handler
    React.useEffect (subscribe, [| |])
    Html.none)
let private handScoreEvent e player1 player2 showToast = handScoreEvent' {| e = e ; player1 = player1 ; player2 = player2 ; showToast = showToast |}

let private cribScoreEvent' = React.functionComponent ("CribScoreEvent", fun (props:{| e : IEvent<Player * Hand * Card * list<CribScoreEvent>> ; player1 : PlayerDetails ; player2 : PlayerDetails ; showToast : Toaster.ToastData -> unit |}) ->
    let subscribe () =
        let handler (player, hand, card, events:CribScoreEvent list) =
            let name = function | Player1 -> props.player1.Name | Player2 -> props.player2.Name
            let score = events |> List.sumBy (fun event -> event.Score)
            // TODO-NMB: More details, e.g. each event.Text (cf. game-player.fs)?...
            let data : Toaster.ToastData = {
                Title = Some "Crib"
                Icon = Some Toaster.Score
                Message = sprintf "%s scores %i" (name player) score
                Purpose = Toaster.Information
                Affinity = Some player
                TimeoutOverride = None }
            data |> props.showToast
        props.e.Add handler
    React.useEffect (subscribe, [| |])
    Html.none)
let private cribScoreEvent e player1 player2 showToast = cribScoreEvent' {| e = e ; player1 = player1 ; player2 = player2 ; showToast = showToast |}

let private game' = React.memo ("Game", fun (props:{| showToast : Toaster.ToastData -> unit |}) ->
    let gameDetails, setGameDetails : GameDetails option * (GameDetails option -> unit) = React.useState (None)
    (* IMPORTANT NOTE: Do *not* call "Theme.useStyles ()" as this causes a re-render (despite memo-ization) when light/dark theme changed - which seems to screw up state / adaptive stuff. *)
    match gameDetails with
    | Some gameDetails ->
        // TODO-NMB: Rework for proper game UI...
        Mui.card [
            card.raised true
            prop.children [
                scores (gameDetails.GameEngine.Scores, gameDetails.Player1Details, gameDetails.Player2Details)
                crib gameDetails.GameEngine.Crib
                cutCard gameDetails.GameEngine.CutCard
                Mui.divider []
                awaitingForCrib (gameDetails.GameEngine.AwaitingForCrib(Player1), gameDetails.Player1Details)
                awaitingForCrib (gameDetails.GameEngine.AwaitingForCrib(Player2), gameDetails.Player2Details)
                awaitingPeg (gameDetails.GameEngine.AwaitingPeg(Player1), gameDetails.Player1Details)
                awaitingPeg (gameDetails.GameEngine.AwaitingPeg(Player2), gameDetails.Player2Details)
                awaitingCannotPeg (gameDetails.GameEngine.AwaitingCannotPeg(Player1), gameDetails.Player1Details)
                awaitingCannotPeg (gameDetails.GameEngine.AwaitingCannotPeg(Player2), gameDetails.Player2Details)
                awaitingNewDeal (gameDetails.GameEngine.AwaitingNewDeal(Player1), gameDetails.Player1Details)
                awaitingNewDeal (gameDetails.GameEngine.AwaitingNewDeal(Player2), gameDetails.Player2Details)
                awaitingNewGame gameDetails.GameEngine
                dealer gameDetails.GameEngine.Dealer gameDetails.Player1Details gameDetails.Player2Details props.showToast
                nibsScoreEvent gameDetails.GameEngine.NibsScoreEvent gameDetails.Player1Details gameDetails.Player2Details props.showToast
                peggingScoreEvent gameDetails.GameEngine.PeggingScoreEvent gameDetails.Player1Details gameDetails.Player2Details props.showToast
                handScoreEvent gameDetails.GameEngine.HandScoreEvent gameDetails.Player1Details gameDetails.Player2Details props.showToast
                cribScoreEvent gameDetails.GameEngine.CribScoreEvent gameDetails.Player1Details gameDetails.Player2Details props.showToast
        ] ]
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
