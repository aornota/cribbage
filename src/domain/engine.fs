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
open System

type Player = | Player1 | Player2

exception GameNotFinishedException
exception PlayersHaveSameNameException
exception NoCurrentDealerException
exception DealNotFinishedException
exception HandDoesNotContain6CardsException
exception UnexpectedForCribForHumanPlayerException of Player
exception AlreadyCutException
exception NoCurrentDealException
exception AlreadyScoredException
exception NotCutException
exception UnexpectedNewDealRequestException of Player
exception UnexpectedNewGameRequestException of Player
exception InvalidInteractionException of string

let [<Literal>] private SOURCE = "Domain.Engine"

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

type private Completed = bool

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
    | RequestNewDeal of Player
    | RequestNewGame of Player

type private AwaitingInteraction =
    | AwaitingForCrib of Player list
    | AwaitingPeg of Player * Peggable list
    | AwaitingCannotPeg of Player
    | AwaitingNewDeal of Player list
    | AwaitingNewGame of Player list

type private State =
    | Quit
    | AwaitingInteraction of AwaitingInteraction
    | NewDeal
    | ForCrib1
    | ForCrib2
    | Cut
    | Pegging
    // TODO-NMB: More pegging?...
    | ScoreNonDealerHand
    | ScoreDealerHand
    | ScoreCrib
    | ProcessDeal of PlayerDealSummary * PlayerDealSummary
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
    let currentDealer : cval<Player option> = cval None
    let currentDeal : cval<(PlayerDealSummary * PlayerDealSummary) option> = cval None
    let deck : cval<Deck> = cval []
    let hand1 : cval<Hand> = cval Set.empty
    let hand2 : cval<Hand> = cval Set.empty
    let crib : cval<Crib> = cval Set.empty
    let cutCard : cval<Card option> = cval None
    let nibsEvent : cval<(Player * NibsScoreEvent) option> = cval None
    let peg1 : cval<Hand> = cval Set.empty
    let peg2 : cval<Hand> = cval Set.empty
    // TODO-NMB: More pegging?...
    let nonDealerHandEvents : cval<(Player * Hand * HandScoreEvent list) option> = cval None
    let dealerHandEvents : cval<(Player * Hand * HandScoreEvent list) option> = cval None
    let cribEvents : cval<(Player * Crib * CribScoreEvent list) option> = cval None
    let awaitingNewDeal : cval<Player list> = cval []
    let awaitingNewGame : cval<Player list> = cval []
    let quitter : cval<Player option> = cval None
    let scores = adaptive {
        let! dealSummaries = dealSummaries
        let! currentDeal = currentDeal
        let score1, score2 = dealSummaries |> List.sumBy (fun summary -> summary.Player1Score), dealSummaries |> List.sumBy (fun summary -> summary.Player2Score)
        let score1, score2 =
            match currentDeal with
            | Some (deal1, deal2) -> score1 + deal1.Score, score2 + deal2.Score
            | None -> score1, score2
        return score1, score2 }
    let awaitingInteraction = adaptive {
        // TODO-NMB: Make dependencies dynamic (e.g. only "bind" to awaitingNewDeal &c. if not yet awaiting something else)?...
        let! hand1 = hand1
        let! hand2 = hand2
        // TODO-NMB: Pegging-related...
        let! awaitingNewDeal = awaitingNewDeal
        let! awaitingNewGame = awaitingNewGame
        let awaitingForCrib = [
            if player1.IsInteractive && hand1.Count = DEALT_HAND_COUNT then Player1
            if player2.IsInteractive && hand2.Count = DEALT_HAND_COUNT then Player2 ]
        if awaitingForCrib.Length > 0 then return Some (AwaitingForCrib awaitingForCrib)
        else if false then return None // TODO-NMB: AwaitingPeg | AwaitingCannotPeg | ...
        else if awaitingNewDeal.Length > 0 then return Some (AwaitingNewDeal awaitingNewDeal)
        else if awaitingNewGame.Length > 0 then return Some (AwaitingNewGame awaitingNewGame)
        else return None }
    let state = adaptive {
        // TODO-NMB: Make dependencies dynamic (e.g. only "bind" to awaitingNewGame if not awaiting something else)?...
        let! quitter = quitter
        let! awaitingInteraction = awaitingInteraction
        let! currentDeal = currentDeal
        let! scores = scores
        let! hand1 = hand1
        let! hand2 = hand2
        let! cutCard = cutCard
        let! peg1 = peg1
        let! peg2 = peg2
        let! nonDealerHandEvents = nonDealerHandEvents
        let! dealerHandEvents = dealerHandEvents
        let! cribEvents = cribEvents
        if quitter |> Option.isSome then return Quit
        else
            match awaitingInteraction with
            | Some awaitingInteraction -> return AwaitingInteraction awaitingInteraction
            | None ->
                match currentDeal with
                | None -> return NewDeal
                | Some (deal1, deal2) ->
                    if fst scores < GAME_TARGET && snd scores < GAME_TARGET then
                        if hand1.Count = DEALT_HAND_COUNT && not player1.IsInteractive then return ForCrib1
                        else if hand2.Count = DEALT_HAND_COUNT && not player2.IsInteractive then return ForCrib2
                        else if cutCard |> Option.isNone then return Cut
                        else if peg1.Count > 0 && peg2.Count > 0 then return Pegging
                        // TODO-NMB: More pegging?...
                        else if nonDealerHandEvents |> Option.isNone then return ScoreNonDealerHand
                        else if dealerHandEvents |> Option.isNone then return ScoreDealerHand
                        else if cribEvents |> Option.isNone then return ScoreCrib
                        else return ProcessDeal (deal1, deal2)
                    else return ProcessGame (deal1, deal2) }
    let toPlayer = function | Player1 -> player1 | Player2 -> player2
    let toHand = function | Player1 -> hand1 | Player2 -> hand2
    let isDealer player = match currentDealer.Value with | Some dealer -> dealer = player | None -> raise NoCurrentDealerException
    let dealer () = if isDealer Player1 then Player1 else Player2
    let failIfNotDealtHand (hand:Hand) = if hand.Count <> DEALT_HAND_COUNT then raise HandDoesNotContain6CardsException
    let newDeal () =
        if currentDeal.Value |> Option.isSome then raise DealNotFinishedException
        let newCurrentDealer = match currentDealer.Value with | Some Player1 -> Player2 | Some Player2 -> Player1 | None -> firstDealer.Value
        let isDealer1, isDealer2 = newCurrentDealer = Player1, newCurrentDealer = Player2
        sourcedLogger.Debug("Dealing hands ({name} is dealer)...", (toPlayer newCurrentDealer).Name)
        let newDeck = shuffledDeck ()
        let newDeck, dealt1 = dealToHand DEALT_HAND_COUNT (newDeck, Set.empty)
        let newDeck, dealt2 = dealToHand DEALT_HAND_COUNT (newDeck, Set.empty)
        sourcedLogger.Debug("...dealt to {name1} -> {dealt1}", player1.Name, cardsText dealt1)
        sourcedLogger.Debug("...dealt to {name2} -> {dealt2}", player2.Name, cardsText dealt2)
        transact (fun _ ->
            currentDealer.Value <- Some newCurrentDealer
            currentDeal.Value <- Some(PlayerDealSummary.New(isDealer1), PlayerDealSummary.New(isDealer2))
            deck.Value <- newDeck
            hand1.Value <- dealt1
            hand2.Value <- dealt2
            crib.Value <- Set.empty
            cutCard.Value <- None
            nibsEvent.Value <- None
            peg1.Value <- Set.empty
            peg2.Value <- Set.empty
            // TODO-NMB: More pegging?...
            nonDealerHandEvents.Value <- None
            dealerHandEvents.Value <- None
            cribEvents.Value <- None
            awaitingNewDeal.Value <- []
            awaitingNewGame.Value <- [])
    let handToCrib player forCrib =
        let hand = toHand player
        failIfNotDealtHand hand.Value
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
            failIfNotDealtHand hand.Value
            sourcedLogger.Debug("Choosing cards for crib from {name}...", name)
            handToCrib player (forCribStrategy (isDealer player, hand.Value))
    let cut () =
        if cutCard.Value |> Option.isSome then raise AlreadyCutException
        let newCutCard, newDeck = cut deck.Value
        let newCurrentDeal, newNibsEvent =
            match currentDeal.Value, NibsScoreEvent.Process(newCutCard) with
            | None, _ -> raise NoCurrentDealException
            | _, None -> None, None
            | Some (deal1, deal2), Some event ->
                let dealer, score = dealer (), event.Score
                let deal1 = if dealer = Player1 then { deal1 with NibsScore = Some score } else deal1
                let deal2 = if dealer = Player2 then { deal2 with NibsScore = Some score } else deal2
                Some (deal1, deal2), Some (dealer, event)
        let nibsEventText = match newNibsEvent with | Some (dealer, event) -> sprintf " -> %s scores %s" (toPlayer dealer).Name event.Text | None -> String.Empty
        sourcedLogger.Debug("Cut: {cutCard}{nibsEventText}", cardText newCutCard, nibsEventText)
        transact (fun _ ->
            match newCurrentDeal with | Some newCurrentDeal -> currentDeal.Value <- Some newCurrentDeal | None -> ()
            deck.Value <- newDeck
            cutCard.Value <- Some newCutCard
            match newNibsEvent with | Some newNibsEvent -> nibsEvent.Value <- Some newNibsEvent | None -> ()
            peg1.Value <- hand1.Value
            peg2.Value <- hand2.Value)

    let pegging () = // TEMP-NMB...
        // TODO-NMB: Implement properly...
        let newCurrentDeal =
            match currentDeal.Value with
            | None -> raise NoCurrentDealException
            | Some (deal1, deal2) ->
                let deal1 = { deal1 with PeggingScore = Some (0<point>, true) }
                let deal2 = { deal2 with PeggingScore = Some (0<point>, true) }
                Some (deal1, deal2)
        transact (fun _ ->
            match newCurrentDeal with | Some newCurrentDeal -> currentDeal.Value <- Some newCurrentDeal | None -> ()
            nibsEvent.Value <- None // TODO-NMB: Only reset once first card pegged...
            peg1.Value <- Set.empty
            peg2.Value <- Set.empty)
    // TODO-NMB: More pegging (remember to reset nibsEvent)?...

    let scoreNonDealerHand () =
        if nonDealerHandEvents.Value |> Option.isSome then raise AlreadyScoredException
        let nonDealer = if dealer () = Player1 then Player2 else Player1
        let newCurrentDeal, newNonDealerHandEvents =
            match currentDeal.Value, cutCard.Value with
            | None, _ -> raise NoCurrentDealException
            | _, None -> raise NotCutException
            | Some (deal1, deal2), Some cutCard ->
                let hand = if nonDealer = Player1 then hand1.Value else hand2.Value
                let events = HandScoreEvent.Process(hand, cutCard)
                let score = events |> List.sumBy (fun event -> event.Score)
                sourcedLogger.Debug("Hand: {hand} | {cutCard} -> {name} scores {score}", cardsText hand, cardText cutCard, (toPlayer nonDealer).Name, score)
                events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
                let deal1 = if nonDealer = Player1 then { deal1 with HandScore = Some score } else deal1
                let deal2 = if nonDealer = Player2 then { deal2 with HandScore = Some score } else deal2
                Some (deal1, deal2), Some (nonDealer, hand, events)
        transact (fun _ ->
            match newCurrentDeal with | Some newCurrentDeal -> currentDeal.Value <- Some newCurrentDeal | None -> ()
            match newNonDealerHandEvents with | Some newNonDealerHandEvents -> nonDealerHandEvents.Value <- Some newNonDealerHandEvents | None -> ())
    let scoreDealerHand () =
        if dealerHandEvents.Value |> Option.isSome then raise AlreadyScoredException
        let dealer = dealer ()
        let newCurrentDeal, newDealerHandEvents =
            match currentDeal.Value, cutCard.Value with
            | None, _ -> raise NoCurrentDealException
            | _, None -> raise NotCutException
            | Some (deal1, deal2), Some cutCard ->
                let hand = if dealer = Player1 then hand1.Value else hand2.Value
                let events = HandScoreEvent.Process(hand, cutCard)
                let score = events |> List.sumBy (fun event -> event.Score)
                sourcedLogger.Debug("Hand: {hand} | {cutCard} -> {name} scores {score}", cardsText hand, cardText cutCard, (toPlayer dealer).Name, score)
                events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
                let deal1 = if dealer = Player1 then { deal1 with HandScore = Some score } else deal1
                let deal2 = if dealer = Player2 then { deal2 with HandScore = Some score } else deal2
                Some (deal1, deal2), Some (dealer, hand, events)
        transact (fun _ ->
            match newCurrentDeal with | Some newCurrentDeal -> currentDeal.Value <- Some newCurrentDeal | None -> ()
            match newDealerHandEvents with | Some newDealerHandEvents -> dealerHandEvents.Value <- Some newDealerHandEvents | None -> ())
    let scoreCrib () =
        if cribEvents.Value |> Option.isSome then raise AlreadyScoredException
        let dealer = dealer ()
        let newCurrentDeal, newCribEvents =
            match currentDeal.Value, cutCard.Value with
            | None, _ -> raise NoCurrentDealException
            | _, None -> raise NotCutException
            | Some (deal1, deal2), Some cutCard ->
                let crib = crib.Value
                let events = CribScoreEvent.Process(crib, cutCard)
                let score = events |> List.sumBy (fun event -> event.Score)
                sourcedLogger.Debug("Crib: {crib} | {cutCard} -> {name} scores {score}", cardsText crib, cardText cutCard, (toPlayer dealer).Name, score)
                events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
                let deal1 = if dealer = Player1 then { deal1 with CribScore = Some score } else deal1
                let deal2 = if dealer = Player2 then { deal2 with CribScore = Some score } else deal2
                Some (deal1, deal2), Some (dealer, crib, events)
        transact (fun _ ->
            match newCurrentDeal with | Some newCurrentDeal -> currentDeal.Value <- Some newCurrentDeal | None -> ()
            match newCribEvents with | Some newCribEvents -> cribEvents.Value <- Some newCribEvents | None -> ())
    let processDeal deal1 deal2 =
        let dealSummary = { Player1DealSummary = deal1 ; Player2DealSummary = deal2 }
        sourcedLogger.Debug("...deal finished -> {name1} scored {score1} and {name2} scored {score2}", player1.Name, deal1.Score, player2.Name, deal2.Score)
        let newDealSummaries = dealSummary :: dealSummaries.Value
        let newAwaitingNewDeal = [
            if player1.IsInteractive then Player1
            if player2.IsInteractive then Player2 ]
        transact (fun _ ->
            dealSummaries.Value <- newDealSummaries
            currentDeal.Value <- None
            awaitingNewDeal.Value <- newAwaitingNewDeal)
    let processGame deal1 deal2 =
        let dealSummary = { Player1DealSummary = deal1 ; Player2DealSummary = deal2 }
        let newDealSummaries = dealSummary :: dealSummaries.Value
        let gameSummary = GameSummary.FromDealSummaries(newDealSummaries)
        sourcedLogger.Debug("...game finished -> {name1} {score1} - {score2} {name2}", player1.Name, gameSummary.Player1Score, gameSummary.Player2Score, player2.Name)
        sourcedLogger.Debug("...{name} wins the game (in {deals} deal/s)", (toPlayer gameSummary.Winner).Name, newDealSummaries.Length)
        let newGameSummaries = gameSummary :: gameSummaries.Value
        let games1, games2 = newGameSummaries |> List.sumBy (fun summary -> summary.Player1Game), newGameSummaries |> List.sumBy (fun summary -> summary.Player2Game)
        sourcedLogger.Debug("...{name1} {games1} - {games2} {name2}", player1.Name, games1, games2, player2.Name)
        let newAwaitingNewGame = [
            if player1.IsInteractive then Player1
            if player2.IsInteractive then Player2 ]
        transact (fun _ ->
            gameSummaries.Value <- newGameSummaries
            dealSummaries.Value <- []
            firstDealer.Value <- if firstDealer.Value = Player1 then Player2 else Player1
            currentDealer.Value <- None
            currentDeal.Value <- None
            awaitingNewGame.Value <- newAwaitingNewGame)
    let requestNewDeal player =
        if not (awaitingNewDeal.Value |> List.contains player) then raise (UnexpectedNewDealRequestException player)
        sourcedLogger.Debug("...new deal requested by {name}", (toPlayer player).Name)
        transact (fun _ -> awaitingNewDeal.Value <- awaitingNewDeal.Value |> List.filter (fun forPlayer -> player <> forPlayer))
    let requestNewGame player =
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
        | AwaitingInteraction (AwaitingNewDeal players) -> players |> List.iter (fun player -> sourcedLogger.Debug("Awaiting new deal request from {name}...", (toPlayer player).Name))
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
        | Cut ->
            cut ()
            engine ()
        | Pegging ->
            pegging ()
            engine ()
        // TODO-NMB: More pegging?...
        | ScoreNonDealerHand ->
            scoreNonDealerHand ()
            engine ()
        | ScoreDealerHand ->
            scoreDealerHand ()
            engine ()
        | ScoreCrib ->
            scoreCrib ()
            engine ()
        | ProcessDeal (deal1, deal2) ->
            processDeal deal1 deal2
            engine ()
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
        | RequestNewDeal player, Some (AwaitingNewDeal players) when players |> List.contains player ->
            requestNewDeal player
            engine ()
        | RequestNewGame player, Some (AwaitingNewGame players) when players |> List.contains player ->
            requestNewGame player
            engine ()
        | _ -> raise (InvalidInteractionException (sprintf "%A when %A" interaction awaitingInteraction))
    member _.Start() = engine ()
    member _.Players = player1, player2
    member _.Games = adaptive {
        let! gameSummaries = gameSummaries
        return gameSummaries |> List.sumBy (fun summary -> summary.Player1Game), gameSummaries |> List.sumBy (fun summary -> summary.Player2Game) }
    member _.Scores = scores
    member _.Dealer = currentDealer
    member _.AwaitingForCrib(player) = adaptive {
        let! currentDealer = currentDealer
        let! hand = toHand player
        let! awaitingInteraction = awaitingInteraction
        match awaitingInteraction with
        | Some (AwaitingForCrib players) when players |> List.contains player ->
            let isDealer = match currentDealer with | Some dealer when dealer = player -> true | _ -> false
            return Some (isDealer, hand, fun forCrib -> interact (ForCrib (player, forCrib)))
        | _ -> return None }
    member _.AwaitingCrib1 = adaptive {
        let! hand1 = hand1
        return hand1.Count = DEALT_HAND_COUNT }
    member _.AwaitingCrib2 = adaptive {
        let! hand2 = hand2
        return hand2.Count = DEALT_HAND_COUNT }
    member _.CutCard = cutCard
    member _.NibsEvent = nibsEvent
    // TODO-NMB: AwaitingPeg (return (Pagged * Peggable * (Card option -> unit)) option) | AwaitingCannotPeg | ...
    member _.NonDealerHandEvents = nonDealerHandEvents
    member _.DealerHandEvents = dealerHandEvents
    member _.CribEvents = cribEvents
    member _.AwaitingNewDeal(player) = adaptive {
        let! awaitingInteraction = awaitingInteraction
        match awaitingInteraction with
        | Some (AwaitingNewDeal players) when players |> List.contains player -> return Some (fun () -> interact (RequestNewDeal player))
        | _ -> return None }
    member _.AwaitingNewGame(player) = adaptive {
        let! awaitingInteraction = awaitingInteraction
        match awaitingInteraction with
        | Some (AwaitingNewGame players) when players |> List.contains player -> return Some (fun () -> interact (RequestNewGame player))
        | _ -> return None }
    member _.Quit(player) = quit player
    member _.Statistics(player) = adaptive {
        let! gameSummaries = gameSummaries
        let playerGameSummaries = gameSummaries |> List.map (fun summary -> if player = Player1 then summary.Player1GameSummary else summary.Player2GameSummary)
        let zero = { Total = 0<point> ; Count = 0 }
        let peggingMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.PeggingMean)) zero
        let peggingDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.PeggingDealerMean)) zero
        let peggingNotDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.PeggingNotDealerMean)) zero
        let handMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.HandMean)) zero
        let handDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.HandDealerMean)) zero
        let handNotDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.HandNotDealerMean)) zero
        let cribMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.CribMean)) zero
        return peggingMean, peggingDealerMean, peggingNotDealerMean, handMean, handDealerMean, handNotDealerMean, cribMean }
