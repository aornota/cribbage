module Aornota.Cribbage.Domain.Engine

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

exception GameNotFinishedException
exception PlayersHaveSameNameException
exception NoCurrentDealerException
exception DealNotFinishedException
exception HandDoesNotContain6CardsException
exception UnexpectedForCribForHumanPlayerException of Player
exception UnexpectedNewGameRequestException of Player
exception InvalidInteractionException of string

let [<Literal>] private SOURCE = "Domain.Engine"

let [<Literal>] private GAME_TARGET = 121<point>

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

type private Completed = bool

// TODO-NMB: More static helper methods for [Player][Game|Deal]Summary?...

type private PlayerDealSummary = {
    WasDealer : IsDealer
    NibsScore : int<point> option
    PeggingScore : (int<point> * Completed) option
    HandScore : int<point> option
    CribScore : int<point> option }
    with
    static member New(isDealer) = {
        WasDealer = isDealer
        NibsScore = None
        PeggingScore = None
        HandScore = None
        CribScore = None }
    member this.Score =
        let defaultValue score = score |> Option.defaultValue 0<point>
        let defaultPegging score = match score with | Some (score, _) -> score | None -> 0<point>
        defaultValue this.NibsScore + defaultPegging this.PeggingScore + defaultValue this.HandScore + defaultValue this.CribScore

type private DealSummary = {
    Player1DealSummary : PlayerDealSummary
    Player2DealSummary : PlayerDealSummary }
    with
    member this.Player1Score = this.Player1DealSummary.Score
    member this.Player2Score = this.Player2DealSummary.Score

type private PlayerGameSummary = {
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

type private GameSummary = {
    Player1GameSummary : PlayerGameSummary
    Player2GameSummary : PlayerGameSummary }
    with
    static member FromDealSummaries(deals:DealSummary list) =
        let player1Score, player2Score = deals |> List.sumBy (fun deal -> deal.Player1DealSummary.Score), deals |> List.sumBy (fun deal -> deal.Player2DealSummary.Score)
        if player1Score < GAME_TARGET && player2Score < GAME_TARGET then raise GameNotFinishedException
        {
            Player1GameSummary = PlayerGameSummary.FromPlayerDealSummaries(deals |> List.map (fun deal -> deal.Player1DealSummary))
            Player2GameSummary = PlayerGameSummary.FromPlayerDealSummaries(deals |> List.map (fun deal -> deal.Player2DealSummary))
        }
    member this.Player1Score = this.Player1GameSummary.Score
    member this.Player2Score = this.Player2GameSummary.Score
    member this.Winner = if this.Player1Score > this.Player2Score then Player1 else Player2
    member this.Player1Game = if this.Winner = Player1 then 1<game> else 0<game>
    member this.Player2Game = if this.Winner = Player2 then 1<game> else 0<game>

type private Interaction =
    | ForCrib of Player * CardS
    | Peg of Player * Card option
    | CannotPeg of Player
    | NewGame of Player

type private AwaitingInteraction =
    | AwaitingForCrib of Player list
    | AwaitingPeg of Player * Peggable list
    | AwaitingCannotPeg of Player
    | AwaitingNewGame of Player list

type private State =
    | Quit
    | AwaitingInteraction of AwaitingInteraction
    | NewDeal
    | ForCrib1
    | ForCrib2
    // TODO-NMB: More...
    | ToDo // TEMP-NMB
    | ProcessGame of PlayerDealSummary * PlayerDealSummary

type PlayerDetails =
    | Human of string
    | Computer of string * ForCribStrategy * PegStrategy
    with
    member this.IsInteractive = match this with | Human _ -> true | Computer _ -> false
    member this.Name = match this with | Human name | Computer (name, _, _) -> name

type Engine (player1:PlayerDetails, player2:PlayerDetails) =
    do if player1.Name = player2.Name then raise PlayersHaveSameNameException
    let sourcedLogger = SourcedLogger()
    do sourcedLogger.Debug("Initializing for {name1} vs. {name2}...", player1.Name, player2.Name)
    let gameSummaries : cval<GameSummary list> = cval []
    let dealSummaries : cval<DealSummary list> = cval []
    let firstDealer = cval (if normalizedRandom () < 0.5 then Player1 else Player2)
    let dealer : cval<Player option> = cval None
    let deal : cval<(PlayerDealSummary * PlayerDealSummary) option> = cval None
    let deck : cval<Deck> = cval []
    let hand1 : cval<Hand> = cval Set.empty
    let hand2 : cval<Hand> = cval Set.empty
    let crib : cval<Crib> = cval Set.empty
    let cut : cval<Card option> = cval None
    // TODO-NMB: Pegging...
    let awaitingNewGame : cval<Player list> = cval []
    let quitter : cval<Player option> = cval None
    let games = adaptive {
        let! gameSummaries = gameSummaries
        return gameSummaries |> List.sumBy (fun summary -> summary.Player1Game), gameSummaries |> List.sumBy (fun summary -> summary.Player2Game) }
    let scores = adaptive {
        let! dealSummaries = dealSummaries
        return dealSummaries |> List.sumBy (fun summary -> summary.Player1Score), dealSummaries |> List.sumBy (fun summary -> summary.Player2Score) }
    let awaitingInteraction = adaptive {
        // TODO-NMB: Make dependencies dynamic (e.g. only "bind" to awaitingNewGame if not awaiting something else)?...
        let! hand1 = hand1
        let! hand2 = hand2
        // TODO-NMB: Pegging-related...
        let! awaitingNewGame = awaitingNewGame
        let awaitingForCrib = [
            if player1.IsInteractive && hand1.Count = 6 then Player1
            if player2.IsInteractive && hand2.Count = 6 then Player2 ]
        if awaitingForCrib.Length > 0 then return Some (AwaitingForCrib awaitingForCrib)
        else if awaitingNewGame.Length > 0 then return Some (AwaitingNewGame awaitingNewGame)
        else
            // TODO-NMB: AwaitingPeg | AwaitingCannotPeg | ...
            return None // TEMP-NMB...
    }
    let state = adaptive {
        // TODO-NMB: Make dependencies dynamic (e.g. only "bind" to awaitingNewGame if not awaiting something else)?...
        let! quitter = quitter
        let! awaitingInteraction = awaitingInteraction
        let! deal = deal
        let! hand1 = hand1
        let! hand2 = hand2
        // TODO-NMB: Pegging-related...
        match quitter with
        | Some _ -> return Quit
        | None ->
            match awaitingInteraction with
            | Some awaitingInteraction -> return AwaitingInteraction awaitingInteraction
            | None ->
                match deal with
                | None -> return NewDeal
                | Some (deal1, deal2) ->
                    if deal1.Score < GAME_TARGET && deal2.Score < GAME_TARGET then
                        if hand1.Count = 6 && not player1.IsInteractive then return ForCrib1
                        else if hand2.Count = 6 && not player2.IsInteractive then return ForCrib2
                        else
                            // TODO-NMB: Cut | Pegging | Scoring | NextDeal | ...

                            return ToDo // TEMP-NMB
                    else return ProcessGame (deal1, deal2) }
    let toPlayer = function | Player1 -> player1 | Player2 -> player2
    let toHand = function | Player1 -> hand1 | Player2 -> hand2
    let isDealer player = match dealer.Value with | Some dealer -> dealer = player | None -> raise NoCurrentDealerException
    let failIfNot6Cards (hand:Hand) = if hand.Count <> 6 then raise HandDoesNotContain6CardsException
    let newDeal () =
        if deal.Value |> Option.isSome then raise DealNotFinishedException
        let newDealer = match dealer.Value with | Some Player1 -> Player2 | Some Player2 -> Player1 | None -> firstDealer.Value
        let isDealer1, isDealer2 = newDealer = Player1, newDealer = Player2
        sourcedLogger.Debug("Dealing hands ({name} is dealer)...", (toPlayer newDealer).Name)
        let newDeck = shuffledDeck ()
        let newDeck, dealt1 = dealToHand 6 (newDeck, Set.empty)
        let newDeck, dealt2 = dealToHand 6 (newDeck, Set.empty)
        sourcedLogger.Debug("...dealt to {name1} -> {dealt1}", player1.Name, cardsText dealt1)
        sourcedLogger.Debug("...dealt to {name2} -> {dealt2}", player2.Name, cardsText dealt2)
        transact (fun _ ->
            dealer.Value <- Some newDealer
            deal.Value <- Some(PlayerDealSummary.New(isDealer1), PlayerDealSummary.New(isDealer2))
            deck.Value <- newDeck
            hand1.Value <- dealt1
            hand2.Value <- dealt2
            crib.Value <- Set.empty)
    let handToCrib player forCrib =
        let hand = toHand player
        failIfNot6Cards hand.Value
        let newHand, newCrib = removeFromHand (hand.Value, forCrib), addToCrib (crib.Value, forCrib)
        sourcedLogger.Debug("...{name} adds {forCrib} to crib -> hand is {newHand}", (toPlayer player).Name, cardsText forCrib, cardsText newHand)
        transact (fun _ ->
            hand.Value <- newHand
            crib.Value <- newCrib)
    let forCrib player =
        match toPlayer player with
        | Human _ -> raise (UnexpectedForCribForHumanPlayerException player)
        | Computer (name, forCribStrategy, _) ->
            let hand = toHand player
            failIfNot6Cards hand.Value
            sourcedLogger.Debug("Choosing cards for crib from {name}...", name)
            handToCrib player (forCribStrategy (isDealer player, hand.Value))

    // TODO-NMB: Cut | Pegging | Scoring | ProcessDeal | ...

    let processGame deal1 deal2 =
        let dealSummary = { Player1DealSummary = deal1 ; Player2DealSummary = deal2 }
        let gameSummary = GameSummary.FromDealSummaries(dealSummary :: dealSummaries.Value)
        sourcedLogger.Debug("...game over -> {name1} {score1} - {score2} {name2}", player1.Name, gameSummary.Player1Score, gameSummary.Player2Score, player2.Name)
        sourcedLogger.Debug("...{name} wins the game", (toPlayer gameSummary.Winner).Name)
        let newGameSummaries = gameSummary :: gameSummaries.Value
        let newAwaitingNewGame = [
            if player1.IsInteractive then Player1
            if player2.IsInteractive then Player2 ]
        transact (fun _ ->
            gameSummaries.Value <- newGameSummaries
            dealSummaries.Value <- []
            firstDealer.Value <- if firstDealer.Value = Player1 then Player2 else Player1
            dealer.Value <- None
            deal.Value <- None
            deck.Value <- []
            hand1.Value <- Set.empty
            hand2.Value <- Set.empty
            crib.Value <- Set.empty
            cut.Value <- None
            awaitingNewGame.Value <- newAwaitingNewGame)
    let newGame player =
        if not (awaitingNewGame.Value |> List.contains player) then raise (UnexpectedNewGameRequestException player)
        sourcedLogger.Debug("...new game requested by {name}", (toPlayer player).Name)
        transact (fun _ -> awaitingNewGame.Value <- awaitingNewGame.Value |> List.filter (fun forPlayer -> player <> forPlayer))
    let quit player =
        sourcedLogger.Debug("...{name} has quit", (toPlayer player).Name)
        transact (fun _ -> quitter.Value <- Some player)
    let rec engine () =
        match state |> AVal.force with
        | Quit -> ()
        | AwaitingInteraction (AwaitingForCrib players) -> players |> List.iter (fun player -> sourcedLogger.Debug("Awaiting cards for crib from {name}...", (toPlayer player).Name))
        | AwaitingInteraction (AwaitingPeg (player, _)) -> sourcedLogger.Debug("Awaiting pegging card from {name}...", (toPlayer player).Name)
        | AwaitingInteraction (AwaitingCannotPeg player) -> sourcedLogger.Debug("Awaiting cannot peg from {name}...", (toPlayer player).Name)
        | AwaitingInteraction (AwaitingNewGame players) -> players |> List.iter (fun player -> sourcedLogger.Debug("Awaiting new game request from {name}...", (toPlayer player).Name))
        | NewDeal ->
            newDeal ()
            engine ()
        | ForCrib1 ->
            forCrib Player1
            engine ()
        | ForCrib2 ->
            forCrib Player2
            engine ()
        | ToDo ->
            () // TEMP-NMB
        | ProcessGame (deal1, deal2) ->
            processGame deal1 deal2
            engine ()
    and interact interaction =
        let awaitingInteraction = awaitingInteraction |> AVal.force
        match interaction, awaitingInteraction with
        | ForCrib (player, forCrib), Some (AwaitingForCrib players) when players |> List.contains player ->
            handToCrib player forCrib
            engine ()
        // TODO-NMB...| Peg (player, card), Some (AwaitingPeg forPlayer) when player = forPlayer ->
        // TODO-NMB...| CannotPeg player, Some (AwaitingCannotPeg forPlayer) when player = forPlayer ->
        | NewGame player, Some (AwaitingNewGame players) when players |> List.contains player ->
            newGame player
            engine ()
        | _ -> raise (InvalidInteractionException (sprintf "%A when %A" interaction awaitingInteraction))
    member _.Start() = engine ()
    member _.Players = player1, player2
    member _.Games = games
    member _.Scores = scores
    member _.AwaitingForCrib(player) = adaptive { // TODO-NMB: Will the use of player work?...
        let! hand = toHand player
        let! awaitingInteraction = awaitingInteraction
        match awaitingInteraction with
        | Some (AwaitingForCrib players) when players |> List.contains player -> return Some (hand, fun forCrib -> interact (ForCrib (player, forCrib)))
        | _ -> return None }
    member _.Cut = cut
    // TODO-NMB: AwaitingPeg (return (Peggable * function) option?) | AwaitingCannotPeg | ...
    member _.AwaitingNewGame(player) = adaptive {
        let! awaitingInteraction = awaitingInteraction
        match awaitingInteraction with
        | Some (AwaitingNewGame players) when players |> List.contains player -> return Some (fun () -> interact (NewGame player))
        | _ -> return None }
    member _.Quit(player) = quit player
    // TODO-NMB: LastScoreEvent/s? | ...
