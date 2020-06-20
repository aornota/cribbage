module Aornota.Cribbage.Domain.GameEngine

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring
open Aornota.Cribbage.Domain.Strategy

open FSharp.Data.Adaptive
#if FABLE
#else
open Serilog
#endif

type Player = | Player1 | Player2

exception PlayersHaveSameNameException
exception HandDoesNotContain6CardsException
exception GameNotFinishedException
exception UnexpectedInitialInputException
exception UnexpectedForCribInteractiveException of Player
exception UnexpectedNewDealRequestException of Player
exception UnexpectedNewGameRequestException of Player
exception UnexpectedForCribForHumanPlayerException of Player
exception AlreadyCutException
exception NotCutException
exception AlreadyScoredException

let [<Literal>] private SOURCE = "Domain.GameEngine"

let [<Literal>] private GAME_TARGET = 121<point>
let [<Literal>] private DEALT_HAND_COUNT = 6

type private SourcedLogger () =
#if FABLE
#else
    let sourcedLogger = sourcedLogger SOURCE Log.Logger
#endif
    member _.Debug(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE
        ()
#else
        sourcedLogger.Debug(messageTemplate, propertyValues)
#endif
    member _.Information(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE
        ()
#else
        sourcedLogger.Information(messageTemplate, propertyValues)
#endif
    member _.Warning(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE
        ()
#else
        sourcedLogger.Warning(messageTemplate, propertyValues)
#endif
    member _.Error(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE
        ()
#else
        sourcedLogger.Error(messageTemplate, propertyValues)
#endif

type private Interactive =
    | ForCrib of Player * CardS
    | Peg of Player * Card option
    | CannotPeg of Player
    | RequestNewDeal of Player
    | RequestNewGame of Player
    | Quit of Player

type private Input =
    | Interactive of Interactive
    | ForCribNonInteractive of Player
    | Cut
    | Pegging // TODO-NMB: More pegging?...
    | ScoreNonDealerHand
    | ScoreDealerHand
    | ScoreCrib
    | NewDeal
    | ProcessGame
    | NewGame

// TODO-NMB: PeggingState...

type private CurrentDeal = {
    Dealer : Player
    Deck : Deck
    DealerHand : Hand
    NonDealerHand : Hand
    Crib : Crib
    CutCard : Card option
    NibsScore : int<point> option
    // TODO-NMB: PeggingState (and scores)...
    DealerHandScore : int<point> option
    NonDealerHandScore : int<point> option
    CribScore : int<point> option }
    with
    member this.IsDealer(player) = this.Dealer = player
    member this.Hand(player) = if this.IsDealer(player) then this.DealerHand else this.NonDealerHand
    member this.UpdateHand(player, hand, crib) = if this.IsDealer(player) then { this with DealerHand = hand ; Crib = crib } else { this with NonDealerHand = hand; Crib = crib }
    member this.Score(player) =
        let defaultValue score = score |> Option.defaultValue 0<point>
        // TODO-NMB: Pegging scores...
        if this.IsDealer(player) then defaultValue this.NibsScore + defaultValue this.DealerHandScore + defaultValue this.CribScore
        else defaultValue this.NonDealerHandScore

type Completed = bool

type PlayerDealSummary = {
    WasDealer : IsDealer
    NibsScore : int<point> option
    PeggingScore : (int<point> * Completed) option
    HandScore : int<point> option
    CribScore : int<point> option }
    with
    member this.Score =
        let defaultValue score = score |> Option.defaultValue 0<point>
        let defaultPegging score = match score with | Some (score, _) -> score | None -> 0<point>
        defaultValue this.NibsScore + defaultPegging this.PeggingScore + defaultValue this.HandScore + defaultValue this.CribScore

type DealSummary = {
    Player1DealSummary : PlayerDealSummary
    Player2DealSummary : PlayerDealSummary }
    with
    member this.Player1Score = this.Player1DealSummary.Score
    member this.Player2Score = this.Player2DealSummary.Score

type private GameState = {
    FirstDealer : Player
    PreviousDeals : DealSummary list
    CurrentDeal : CurrentDeal }
    with
    member this.ReadyForCut =
        let currentDeal = this.CurrentDeal
        (currentDeal.Hand(Player1)).Count <> DEALT_HAND_COUNT && (currentDeal.Hand(Player2)).Count <> DEALT_HAND_COUNT
    member this.Scores =
        let currentDeal = this.CurrentDeal
        let player1Score = (this.PreviousDeals |> List.sumBy (fun summary -> summary.Player1Score)) + (currentDeal.Score(Player1))
        let player2Score = (this.PreviousDeals |> List.sumBy (fun summary -> summary.Player2Score)) + (currentDeal.Score(Player2))
        player1Score, player2Score
    member this.GameOver =
        let player1Score, player2Score = this.Scores
        player1Score >= GAME_TARGET || player2Score >= GAME_TARGET

type PlayerGameSummary = {
    Score : int<point>
    PeggingDealerMean : Mean<point>
    PeggingNotDealerMean : Mean<point>
    HandDealerMean : Mean<point>
    HandNotDealerMean : Mean<point>
    CribMean : Mean<point> }
    with
    static member FromPlayerDealSummaries(deals:PlayerDealSummary list) =
        let peggingMean wasDealer =
            Mean<_>.FromList(deals |> List.choose (fun deal -> if deal.WasDealer = wasDealer then match deal.PeggingScore with | Some (score, true) -> Some score | _ -> None else None))
        let handMean wasDealer = Mean<_>.FromList(deals |> List.choose (fun deal -> if deal.WasDealer = wasDealer then match deal.HandScore with | Some score -> Some score | _ -> None else None))
        {
            Score = deals |> List.sumBy (fun deal -> deal.Score)
            PeggingDealerMean = peggingMean true
            PeggingNotDealerMean = peggingMean false
            HandDealerMean = handMean true
            HandNotDealerMean = handMean false
            CribMean = Mean<_>.FromList(deals |> List.choose(fun deal -> deal.CribScore))
        }
    member this.PeggingMean = Mean<_>.Combine(this.PeggingDealerMean, this.PeggingNotDealerMean)
    member this.HandMean = Mean<_>.Combine(this.HandDealerMean, this.HandNotDealerMean)

type GameSummary = {
    Player1GameSummary : PlayerGameSummary
    Player2GameSummary : PlayerGameSummary }
    with
    static member FromDealSummaries(deals:DealSummary list) =
        let player1Score, player2Score = deals |> List.sumBy (fun deal -> deal.Player1Score), deals |> List.sumBy (fun deal -> deal.Player2Score)
        if player1Score < GAME_TARGET && player2Score < GAME_TARGET then raise GameNotFinishedException
        {
            Player1GameSummary = PlayerGameSummary.FromPlayerDealSummaries(deals |> List.map (fun deal -> deal.Player1DealSummary))
            Player2GameSummary = PlayerGameSummary.FromPlayerDealSummaries(deals |> List.map (fun deal -> deal.Player2DealSummary))
        }
    member this.Player1Score = this.Player1GameSummary.Score
    member this.Player2Score = this.Player2GameSummary.Score
    member this.IsWinner(player) = (if this.Player1Score > this.Player2Score then Player1 else Player2) = player

type PlayerDetails =
    | Human of string
    | Computer of string * ForCribStrategy * PegStrategy
    with
    member this.IsInteractive = match this with | Human _ -> true | Computer _ -> false
    member this.Name = match this with | Human name | Computer (name, _, _) -> name

type GameEngine(player1:PlayerDetails, player2:PlayerDetails) =
    do if player1.Name = player2.Name then raise PlayersHaveSameNameException
    let player1IsInteractive, player2IsInteractive = player1.IsInteractive, player2.IsInteractive
    let sourcedLogger = SourcedLogger()
    do sourcedLogger.Debug("Initializing for {player1} vs. {player2}...", player1.Name, player2.Name)
    let failIfNotDealtHand (hand:Hand) = if hand.Count <> DEALT_HAND_COUNT then raise HandDoesNotContain6CardsException
    let otherPlayer = function | Player1 -> Player2 | Player2 -> Player1
    let toPlayerDetails = function | Player1 -> player1 | Player2 -> player2
    let newDeal dealer =
        let nonDealer = otherPlayer dealer
        let dealerName, nonDealerName = (toPlayerDetails dealer).Name, (toPlayerDetails nonDealer).Name
        sourcedLogger.Debug("Dealing hands ({dealer} is dealer)...", dealerName)
        let deck = shuffledDeck ()
        let deck, dealerHand = dealToHand DEALT_HAND_COUNT (deck, Set.empty)
        let deck, nonDealerHand = dealToHand DEALT_HAND_COUNT (deck, Set.empty)
        sourcedLogger.Debug("...dealt to {dealer} -> {dealerHand}", dealerName, cardsText dealerHand)
        sourcedLogger.Debug("...dealt to {nonDealer} -> {nonDealerHand}", nonDealerName, cardsText nonDealerHand)
        {
            Dealer = dealer
            Deck = deck
            DealerHand = dealerHand
            NonDealerHand = nonDealerHand
            Crib = Set.empty
            CutCard = None
            NibsScore = None
            // TODO-NMB: PeggingState (and scores)...
            DealerHandScore = None
            NonDealerHandScore = None
            CribScore = None
        }
    let newGame dealer =
        sourcedLogger.Debug("Starting game ({dealer} is first dealer)...", (toPlayerDetails dealer).Name)
        {
            FirstDealer = dealer
            PreviousDeals = []
            CurrentDeal = newDeal dealer
        }
    let gameState = cval (newGame (if normalizedRandom () < 0.5 then Player1 else Player2))
    let awaitingForCribPlayer1 : cval<(bool * Hand) option> = cval None
    let awaitingForCribPlayer2 : cval<(bool * Hand) option> = cval None
    // TODO-NMB: awaitingPegPlayer[1|2] | awaitingCannotPegPlayer[1|2] | ...
    let awaitingNewDealPlayer1 = cval false
    let awaitingNewDealPlayer2 = cval false
    let awaitingNewGamePlayer1 = cval false
    let awaitingNewGamePlayer2 = cval false
    let forCribNonInteractive () =
        let currentDeal = gameState.Value.CurrentDeal
        if not player1IsInteractive && (currentDeal.Hand(Player1)).Count = DEALT_HAND_COUNT then Some (ForCribNonInteractive Player1)
        else if not player2IsInteractive && (currentDeal.Hand(Player2)).Count = DEALT_HAND_COUNT then Some (ForCribNonInteractive Player2)
        else None
    let awaitingForCribInteractive player =
        let currentDeal = gameState.Value.CurrentDeal
        if player = Player1 && player1IsInteractive then
            match awaitingForCribPlayer1.Value with
            | Some _ -> ()
            | None ->
                let hand = currentDeal.Hand(Player1)
                if hand.Count = DEALT_HAND_COUNT then
                    sourcedLogger.Debug("Awaiting cards for crib from {player}...", (toPlayerDetails Player1).Name)
                    transact (fun _ -> awaitingForCribPlayer1.Value <- Some (currentDeal.IsDealer(Player1), hand))
        else if player2IsInteractive then
            match awaitingForCribPlayer2.Value with
            | Some _ -> ()
            | None ->
                let hand = currentDeal.Hand(Player2)
                if hand.Count = DEALT_HAND_COUNT then
                    sourcedLogger.Debug("Awaiting cards for crib from {player}...", (toPlayerDetails Player2).Name)
                    transact (fun _ -> awaitingForCribPlayer2.Value <- Some (currentDeal.IsDealer(Player2), hand))
    let handToCrib (currentDeal:CurrentDeal) player forCrib =
        let hand = currentDeal.Hand(player)
        failIfNotDealtHand hand
        let hand, crib = removeFromHand (hand, forCrib), addToCrib (currentDeal.Crib, forCrib)
        sourcedLogger.Debug("...{player} adds {forCrib} to crib -> hand is {hand}", (toPlayerDetails player).Name, cardsText forCrib, cardsText hand)
        hand, crib
    let dealSummary (currentDeal:CurrentDeal) =
        let player1WasDealer = currentDeal.IsDealer(Player1)
        let player1DealSummary = {
            WasDealer = player1WasDealer
            NibsScore = if player1WasDealer then currentDeal.NibsScore else None
            // TODO-NMB: Calculate PeggingScore correctly...
            PeggingScore = Some (0<point>, true) // TEMP-NMB: Not None (else error in PlayerGameSummary.FromPlayerDealSummaries(...))...
            HandScore = if player1WasDealer then currentDeal.DealerHandScore else currentDeal.NonDealerHandScore
            CribScore = if player1WasDealer then currentDeal.CribScore else None }
        let player2DealSummary = {
            WasDealer = not player1WasDealer
            NibsScore = if not player1WasDealer then currentDeal.NibsScore else None
            // TODO-NMB: Calculate PeggingScore correctly...
            PeggingScore = Some (0<point>, true) // TEMP-NMB: Not None (else error in PlayerGameSummary.FromPlayerDealSummaries(...))...
            HandScore = if not player1WasDealer then currentDeal.DealerHandScore else currentDeal.NonDealerHandScore
            CribScore = if not player1WasDealer then currentDeal.CribScore else None }
        {
            Player1DealSummary = player1DealSummary
            Player2DealSummary = player2DealSummary
        }
    let nibsScoreEvent = new Event<Player * Card * NibsScoreEvent>()
    // TODO-NMB: Pegging event/s...
    let handScoreEvent = new Event<Player * Hand * Card * HandScoreEvent list>()
    let cribScoreEvent = new Event<Player * Crib * Card * CribScoreEvent list>()
    let gameOverEvent = new Event<GameSummary> ()
    let agent = MailboxProcessor<_>.Start(fun inbox ->
        let rec loop (initialInput:Input option) = async {
            match initialInput with
            | Some (ForCribNonInteractive player) ->
                match toPlayerDetails player with
                | Human _ -> raise (UnexpectedForCribForHumanPlayerException player)
                | Computer (name, forCribStrategy, _) ->
                    let currentDeal = gameState.Value.CurrentDeal
                    let hand = currentDeal.Hand(player)
                    failIfNotDealtHand hand
                    sourcedLogger.Debug("Choosing cards for crib from {player}...", name)
                    let hand, crib = handToCrib currentDeal player (forCribStrategy (currentDeal.IsDealer(player), hand))
                    transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = currentDeal.UpdateHand(player, hand, crib) })
                    match forCribNonInteractive () with
                    | Some input -> inbox.Post input
                    | None -> if gameState.Value.ReadyForCut then inbox.Post Cut
                    return! loop None
            | Some _ -> raise UnexpectedInitialInputException
            | None ->
                match awaitingForCribPlayer1.Value with
                | Some _ -> sourcedLogger.Debug("Awaiting cards for crib from {player}...", (toPlayerDetails Player1).Name)
                | None -> awaitingForCribInteractive Player1
                match awaitingForCribPlayer2.Value with
                | Some _ -> sourcedLogger.Debug("Awaiting cards for crib from {player}...", (toPlayerDetails Player2).Name)
                | None -> awaitingForCribInteractive Player2
                // TODO-NMB: awaitingPegPlayer[1|2] | awaitingCannotPegPlayer[1|2] | ...
                if awaitingNewDealPlayer1.Value then sourcedLogger.Debug("Awaiting new deal request from {player}...", (toPlayerDetails Player1).Name)
                if awaitingNewDealPlayer2.Value then sourcedLogger.Debug("Awaiting new deal request from {player}...", (toPlayerDetails Player2).Name)
                if awaitingNewGamePlayer1.Value then sourcedLogger.Debug("Awaiting new game request from {player}...", (toPlayerDetails Player1).Name)
                if awaitingNewGamePlayer2.Value then sourcedLogger.Debug("Awaiting new game request from {player}...", (toPlayerDetails Player2).Name)
                match! inbox.Receive() with
                | Interactive (ForCrib (player, forCrib)) ->
                    let currentDeal = gameState.Value.CurrentDeal
                    if (if player = Player1 then awaitingForCribPlayer1 else awaitingForCribPlayer2).Value |> Option.isNone then raise (UnexpectedForCribInteractiveException player)
                    let hand, crib = handToCrib currentDeal player forCrib
                    transact (fun _ ->
                        gameState.Value <- { gameState.Value with CurrentDeal = currentDeal.UpdateHand(player, hand, crib) }
                        (if player = Player1 then awaitingForCribPlayer1 else awaitingForCribPlayer2).Value <- None)
                    if gameState.Value.ReadyForCut then inbox.Post Cut
                    return! loop None
                | Interactive (Peg (player, card)) ->
                    // TODO-NMB...
                    return! loop None
                | Interactive (CannotPeg player) ->
                    // TODO-NMB...
                    return! loop None
                | Interactive (RequestNewDeal player) ->
                    if not (if player = Player1 then awaitingNewDealPlayer1 else awaitingNewDealPlayer2).Value then raise (UnexpectedNewDealRequestException player)
                    sourcedLogger.Debug("...new deal requested by {name}", (toPlayerDetails player).Name)
                    transact (fun _ -> (if player = Player1 then awaitingNewDealPlayer1 else awaitingNewDealPlayer2).Value <- false)
                    if not awaitingNewDealPlayer1.Value && not awaitingNewDealPlayer2.Value then inbox.Post NewDeal
                    return! loop None
                | Interactive (RequestNewGame player) ->
                    if not (if player = Player1 then awaitingNewGamePlayer1 else awaitingNewGamePlayer2).Value then raise (UnexpectedNewGameRequestException player)
                    sourcedLogger.Debug("...new game requested by {name}", (toPlayerDetails player).Name)
                    transact (fun _ -> (if player = Player1 then awaitingNewGamePlayer1 else awaitingNewGamePlayer2).Value <- false)
                    if not awaitingNewGamePlayer1.Value && not awaitingNewGamePlayer2.Value then inbox.Post NewGame
                    return! loop None
                | Interactive (Quit player) ->
                    sourcedLogger.Debug("...{name} has quit", (toPlayerDetails player).Name)
                    return ()
                | ForCribNonInteractive player ->
                    match toPlayerDetails player with
                    | Human _ -> raise (UnexpectedForCribForHumanPlayerException player)
                    | Computer (name, forCribStrategy, _) ->
                        let currentDeal = gameState.Value.CurrentDeal
                        let hand = currentDeal.Hand(player)
                        failIfNotDealtHand hand
                        sourcedLogger.Debug("Choosing cards for crib from {player}...", name)
                        let hand, crib = handToCrib currentDeal player (forCribStrategy (currentDeal.IsDealer(player), hand))
                        transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = currentDeal.UpdateHand(player, hand, crib) })
                        match forCribNonInteractive () with
                        | Some input -> inbox.Post input
                        | None -> if gameState.Value.ReadyForCut then inbox.Post Cut
                        return! loop None
                | Cut ->
                    let currentDeal = gameState.Value.CurrentDeal
                    if currentDeal.CutCard |> Option.isSome then raise AlreadyCutException
                    let cutCard, deck = cut currentDeal.Deck
                    let currentDeal =
                        match NibsScoreEvent.Process(cutCard) with
                        | Some event ->
                            sourcedLogger.Debug("Cut: {cutCard} -> {dealer} scores {event}", cardText cutCard, (toPlayerDetails currentDeal.Dealer).Name, event.Text)
                            nibsScoreEvent.Trigger(currentDeal.Dealer, cutCard, event)
                            { currentDeal with NibsScore = Some event.Score }
                        | None ->
                            sourcedLogger.Debug("Cut: {cutCard}", cardText cutCard)
                            currentDeal
                    transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = { currentDeal with Deck = deck ; CutCard = Some cutCard } })
                    if gameState.Value.GameOver then inbox.Post ProcessGame
                    else
                        // TODO-NMB: Start pegging "loop"...
                        inbox.Post ScoreNonDealerHand // TEMP-NMB...
                    return! loop None
                | Pegging ->
                    // TODO-NMB...
                    return! loop None // TEMP-NMB...
                | ScoreNonDealerHand ->
                    let currentDeal = gameState.Value.CurrentDeal
                    let currentDeal =
                        match currentDeal.CutCard, currentDeal.NonDealerHandScore with
                        | None, _ -> raise NotCutException
                        | Some _, Some _ -> raise AlreadyScoredException
                        | Some cutCard, None ->
                            let nonDealer, hand = (if currentDeal.IsDealer(Player1) then Player2 else Player1), currentDeal.NonDealerHand
                            let events = HandScoreEvent.Process(hand, cutCard)
                            let score = events |> List.sumBy (fun event -> event.Score)
                            sourcedLogger.Debug("Hand: {hand} | {cutCard} -> {nonDealer} scores {score}", cardsText hand, cardText cutCard, (toPlayerDetails nonDealer).Name, score)
                            events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
                            handScoreEvent.Trigger(nonDealer, hand, cutCard, events)
                            { currentDeal with NonDealerHandScore = Some score }
                    transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = currentDeal })
                    if gameState.Value.GameOver then inbox.Post ProcessGame else inbox.Post ScoreDealerHand
                    return! loop None
                | ScoreDealerHand ->
                    let currentDeal = gameState.Value.CurrentDeal
                    let currentDeal =
                        match currentDeal.CutCard, currentDeal.DealerHandScore with
                        | None, _ -> raise NotCutException
                        | Some _, Some _ -> raise AlreadyScoredException
                        | Some cutCard, None ->
                            let dealer, hand = currentDeal.Dealer, currentDeal.DealerHand
                            let events = HandScoreEvent.Process(hand, cutCard)
                            let score = events |> List.sumBy (fun event -> event.Score)
                            sourcedLogger.Debug("Hand: {hand} | {cutCard} -> {dealer} scores {score}", cardsText hand, cardText cutCard, (toPlayerDetails dealer).Name, score)
                            events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
                            handScoreEvent.Trigger(dealer, hand, cutCard, events)
                            { currentDeal with DealerHandScore = Some score }
                    transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = currentDeal })
                    if gameState.Value.GameOver then inbox.Post ProcessGame else inbox.Post ScoreCrib
                    return! loop None
                | ScoreCrib ->
                    let currentDeal = gameState.Value.CurrentDeal
                    let currentDeal =
                        match currentDeal.CutCard, currentDeal.CribScore with
                        | None, _ -> raise NotCutException
                        | Some _, Some _ -> raise AlreadyScoredException
                        | Some cutCard, None ->
                            let dealer, crib = currentDeal.Dealer, currentDeal.Crib
                            let events = CribScoreEvent.Process(crib, cutCard)
                            let score = events |> List.sumBy (fun event -> event.Score)
                            sourcedLogger.Debug("Crib: {crib} | {cutCard} -> {dealer} scores {score}", cardsText crib, cardText cutCard, (toPlayerDetails dealer).Name, score)
                            events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
                            cribScoreEvent.Trigger(dealer, crib, cutCard, events)
                            { currentDeal with CribScore = Some score }
                    let player1Score, player2Score = currentDeal.Score(Player1), currentDeal.Score(Player2)
                    sourcedLogger.Debug("...deal finished -> {player1} scored {player1Score} and {player2} scored {player2Score}", player1.Name, player1Score, player2.Name, player2Score)
                    transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = currentDeal })
                    if gameState.Value.GameOver then inbox.Post ProcessGame
                    else
                        transact (fun _ ->
                            awaitingNewDealPlayer1.Value <- player1IsInteractive
                            awaitingNewDealPlayer2.Value <- player2IsInteractive)
                        if not awaitingNewDealPlayer1.Value && not awaitingNewDealPlayer2.Value then inbox.Post NewDeal
                    return! loop None
                | NewDeal->
                    let currentDeal = gameState.Value.CurrentDeal
                    let dealSummary = dealSummary currentDeal
                    transact (fun _ -> gameState.Value <- { gameState.Value with PreviousDeals = dealSummary :: gameState.Value.PreviousDeals ; CurrentDeal = newDeal (otherPlayer currentDeal.Dealer) })
                    match forCribNonInteractive () with | Some input -> inbox.Post input | None -> ()
                    return! loop None
                | ProcessGame ->
                    let player1Score, player2Score = gameState.Value.Scores
                    sourcedLogger.Debug("...game finished -> {player1} {games1} - {games2} {player2}", player1.Name, player1Score, player2Score, player2.Name)
                    let dealSummary = dealSummary gameState.Value.CurrentDeal
                    gameOverEvent.Trigger(GameSummary.FromDealSummaries(dealSummary :: gameState.Value.PreviousDeals))
                    transact (fun _ ->
                        awaitingNewGamePlayer1.Value <- player1IsInteractive
                        awaitingNewGamePlayer2.Value <- player2IsInteractive)
                    if not awaitingNewGamePlayer1.Value && not awaitingNewGamePlayer2.Value then inbox.Post NewGame
                    return! loop None
                | NewGame ->
                    transact (fun _ -> gameState.Value <- newGame (otherPlayer gameState.Value.FirstDealer))
                    match forCribNonInteractive () with | Some input -> inbox.Post input | None -> ()
                    return! loop None }
        loop (forCribNonInteractive ()))
    do agent.Error.Add (fun exn -> sourcedLogger.Error("Unexpected error -> {message}", exn.Message))
    member _.Players = player1, player2
    member _.Scores = gameState |> AVal.map (fun gameState -> gameState.Scores)
    member _.Dealer = gameState |> AVal.map (fun gameState -> gameState.CurrentDeal.Dealer)
    member _.AwaitingForCrib(player) =
        (if player = Player1 then awaitingForCribPlayer1 else awaitingForCribPlayer2)
        |> AVal.map (fun awaiting ->
            match awaiting with
            | Some (isDealer, hand) -> Some (isDealer, hand, fun forCrib -> agent.Post(Interactive (ForCrib (player, forCrib))))
            | None -> None)
    member _.CutCard = gameState |> AVal.map (fun gameState -> gameState.CurrentDeal.CutCard)
    // TODO-NMB: AwaitingPegPlayer[1|2] (return (Pagged * Peggable * (Card option -> unit)) option) | AwaitingCannotPegPlayer[1|2] | ...
    member _.AwaitingNewDeal(player) =
        (if player = Player1 then awaitingNewDealPlayer1 else awaitingNewDealPlayer2)
        |> AVal.map (fun awaiting -> if awaiting then Some (fun () -> agent.Post(Interactive (RequestNewDeal player))) else None)
    member _.AwaitingNewGame(player) =
        (if player = Player1 then awaitingNewGamePlayer1 else awaitingNewGamePlayer2)
        |> AVal.map (fun awaiting -> if awaiting then Some (fun () -> agent.Post(Interactive (RequestNewGame player))) else None)
    member _.Quit(player) = agent.Post(Interactive (Quit player))
    member _.NibsScoreEvent = nibsScoreEvent.Publish
    // TODO-NMB: Pegging event/s...
    member _.HandScoreEvent = handScoreEvent.Publish
    member _.CribScoreEvent = cribScoreEvent.Publish
    member _.GameOverEvent = gameOverEvent.Publish
