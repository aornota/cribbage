module Aornota.Cribbage.Domain.GameEngine

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring
open Aornota.Cribbage.Domain.Strategy

open FSharp.Data.Adaptive
#if FABLE_COMPILER
#else
open Serilog
#endif

type Player = | Player1 | Player2

exception AlreadyCutException
exception AlreadyScoredException
exception GameNotFinishedException
exception HandDoesNotContain6CardsException
exception InvalidCurrentPeggingException
exception NoScoreForGoException
exception NotCutException
exception PlayersHaveSameNameException
exception UnexpectedCannotPegInteractiveException of Player
exception UnexpectedForCribInteractiveException of Player
exception UnexpectedForCribNonInteractiveException of Player
exception UnexpectedInitialInputException
exception UnexpectedNewDealRequestException of Player
exception UnexpectedNewGameRequestException of Player
exception UnexpectedPegException of Player
exception UnexpectedPeggingException
exception UnexpectedPeggingForHumanPlayerException of Player
exception UnexpectedPegInteractiveException of Player

let [<Literal>] private SOURCE = "Domain.GameEngine"

let [<Literal>] private GAME_TARGET = 121<point>
let [<Literal>] private DEALT_HAND_COUNT = 6

type private SourcedLogger () =
#if FABLE_COMPILER
#else
    let sourcedLogger = sourcedLogger SOURCE Log.Logger
#endif
    member _.Debug(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE_COMPILER
        ()
#else
        sourcedLogger.Debug(messageTemplate, propertyValues)
#endif
    member _.Information(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE_COMPILER
        ()
#else
        sourcedLogger.Information(messageTemplate, propertyValues)
#endif
    member _.Warning(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE_COMPILER
        ()
#else
        sourcedLogger.Warning(messageTemplate, propertyValues)
#endif
    member _.Error(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) =
#if FABLE_COMPILER
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
    | Pegging
    | ScoreNonDealerHand
    | ScoreDealerHand
    | ScoreCrib
    | NewDeal
    | ProcessGame
    | NewGame

type private PeggingState = {
    Player1Hand : Hand
    Player2Hand : Hand
    Player1Knocked : bool
    Player2Knocked : bool
    Player1Score : int<point>
    Player2Score : int<point>
    CurrentPegging : (Card * Player) list
    PreviousPegging : ((Card * Player) list) list }
    with
    member this.CurrentPips = pips (this.CurrentPegging |> List.map fst)
    member this.Play(player, card, score) =
        let newHand current = function | Some card -> removeFromHand (current, Set.singleton card) | None -> current
        let newScore current = function | Some score -> current + score | None -> current
        match card with
        | Some (card:Card) ->
            let currentPips = this.CurrentPips
            if currentPips + (fst card).PipValue > MAX_PEGGING then raise (CannotPlayCardException (sprintf "Cannot play %s when running total is %i" (cardText card) (int currentPips)))
        | None -> ()
        let isKnock, currentPegging = match card with | Some card -> false, (card, player) :: this.CurrentPegging | None -> true, this.CurrentPegging
        if player = Player1 then
            { this with
                Player1Hand = newHand this.Player1Hand card
                Player1Knocked = isKnock
                Player1Score = newScore this.Player1Score score
                CurrentPegging = currentPegging }
        else
            { this with
                Player2Hand = newHand this.Player2Hand card
                Player2Knocked = isKnock
                Player2Score = newScore this.Player2Score score
                CurrentPegging = currentPegging }
    member this.PegState(player, cutCard, selfCrib, isDealer) =
        let mapForSelf (pegging:(Card * Player) list) : Pegged = pegging |> List.map (fun (card, player') -> card, player' = player)
        let previouslyPegged = this.PreviousPegging |> List.map mapForSelf
        let pegged = mapForSelf this.CurrentPegging
        let hand = if player = Player1 then this.Player1Hand else this.Player2Hand
        let currentPips = pips (this.CurrentPegging |> List.map fst)
        let peggable, notPeggable = hand |> Set.partition (fun (rank, _) -> currentPips + rank.PipValue <= MAX_PEGGING)
        {
            PreviouslyPegged = previouslyPegged
            Pegged = pegged
            Peggable = peggable
            NotPeggable = notPeggable
            CutCard = cutCard
            SelfCrib = selfCrib
            IsDealer = isDealer
        }
    member this.Completed = this.Player1Hand.Count = 0 && this.Player2Hand.Count = 0 && this.Player1Knocked && this.Player2Knocked

type private CurrentDeal = {
    Dealer : Player
    Deck : Deck
    DealerHand : Hand
    NonDealerHand : Hand
    DealerForCrib : CardS
    NonDealerForCrib : CardS
    CutCard : Card option
    NibsScore : int<point> option
    PeggingState : PeggingState option
    DealerHandScore : int<point> option
    NonDealerHandScore : int<point> option
    CribScore : int<point> option }
    with
    member this.IsDealer(player) : IsDealer = this.Dealer = player
    member this.Hand(player) = if this.IsDealer(player) then this.DealerHand else this.NonDealerHand
    member this.UpdateHand(player, hand, forCrib) =
        if this.IsDealer(player) then { this with DealerHand = hand ; DealerForCrib = forCrib } else { this with NonDealerHand = hand; NonDealerForCrib = forCrib }
    member this.Score(player) =
        let defaultValue score = score |> Option.defaultValue 0<point>
        let peggingScore =
            match this.PeggingState with
            | Some peggingState -> Some (if player = Player1 then peggingState.Player1Score else peggingState.Player2Score)
            | None -> None
        if this.IsDealer(player) then defaultValue this.NibsScore + defaultValue peggingScore + defaultValue this.DealerHandScore + defaultValue this.CribScore
        else defaultValue peggingScore + defaultValue this.NonDealerHandScore
    member this.ForPegState(player) =
        match this.CutCard with
        | Some cutCard -> cutCard, (if this.IsDealer(player) then this.DealerForCrib else this.NonDealerForCrib)
        | None -> raise NotCutException

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
    Deals : int
    Player1GameSummary : PlayerGameSummary
    Player2GameSummary : PlayerGameSummary }
    with
    static member FromDealSummaries(deals:DealSummary list) =
        let player1Score, player2Score = deals |> List.sumBy (fun deal -> deal.Player1Score), deals |> List.sumBy (fun deal -> deal.Player2Score)
        if player1Score < GAME_TARGET && player2Score < GAME_TARGET then raise GameNotFinishedException
        {
            Deals = deals.Length
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
            DealerForCrib = Set.empty
            NonDealerForCrib = Set.empty
            CutCard = None
            NibsScore = None
            PeggingState = None
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
    let awaitingForCribPlayer1 : cval<(IsDealer * Hand) option> = cval None
    let awaitingForCribPlayer2 : cval<(IsDealer * Hand) option> = cval None
    let awaitingPegPlayer1 : cval<PegState option> = cval None
    let awaitingPegPlayer2 : cval<PegState option> = cval None
    let awaitingCannotPegPlayer1 = cval false
    let awaitingCannotPegPlayer2 = cval false
    let awaitingNewDealPlayer1 = cval false
    let awaitingNewDealPlayer2 = cval false
    let awaitingNewGamePlayer1 = cval false
    let awaitingNewGamePlayer2 = cval false
    let nibsScoreEvent = new Event<Player * Card * NibsScoreEvent>()
    let peggingScoreEvent = new Event<Player * Card option * CardL * PeggingScoreEvent list>()
    let handScoreEvent = new Event<Player * Hand * Card * HandScoreEvent list>()
    let cribScoreEvent = new Event<Player * Crib * Card * CribScoreEvent list>()
    let gameOverEvent = new Event<GameSummary> ()
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
                    sourcedLogger.Debug("Awaiting cards for crib from {player}...", player1.Name)
                    transact (fun _ -> awaitingForCribPlayer1.Value <- Some (currentDeal.IsDealer(Player1), hand))
        else if player2IsInteractive then
            match awaitingForCribPlayer2.Value with
            | Some _ -> ()
            | None ->
                let hand = currentDeal.Hand(Player2)
                if hand.Count = DEALT_HAND_COUNT then
                    sourcedLogger.Debug("Awaiting cards for crib from {player}...", player2.Name)
                    transact (fun _ -> awaitingForCribPlayer2.Value <- Some (currentDeal.IsDealer(Player2), hand))
    let handToCrib (currentDeal:CurrentDeal) player forCrib =
        let hand = currentDeal.Hand(player)
        failIfNotDealtHand hand
        let hand = removeFromHand (hand, forCrib)
        sourcedLogger.Debug("...{player} adds {forCrib} to crib -> hand is {hand}", (toPlayerDetails player).Name, cardsText forCrib, cardsText hand)
        hand
    let handleForCribNonInteractive player =
        match toPlayerDetails player with
        | Human _ -> raise (UnexpectedForCribNonInteractiveException player)
        | Computer (name, forCribStrategy, _) ->
            let currentDeal = gameState.Value.CurrentDeal
            let hand = currentDeal.Hand(player)
            failIfNotDealtHand hand
            sourcedLogger.Debug("Choosing cards for crib from {player}...", name)
            let forCrib = forCribStrategy (currentDeal.IsDealer(player), hand)
            let hand = handToCrib currentDeal player forCrib
            transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = currentDeal.UpdateHand(player, hand, forCrib) })
            match forCribNonInteractive () with
            | Some input -> Some input
            | None -> if gameState.Value.ReadyForCut then Some Cut else None
    let peg (peggingState:PeggingState option) player card =
        match peggingState with
        | Some peggingState ->
            let pegged = peggingState.CurrentPegging |> List.rev |> List.map fst
            let previous = match pegged |> List.map cardText with | h :: t -> sprintf "(%s) " (h :: t |> String.concat " ") | [] -> ""
            let played, runningTotal =
                match card with
                | Some card -> Some (cardText card), peggingState.CurrentPips + (fst card).PipValue
                | None -> None, peggingState.CurrentPips
            let score =
                match PeggingScoreEvent.Play(peggingState.CurrentPegging |> List.map fst, card) with
                | h :: t ->
                    let score = h :: t |> List.sumBy (fun event -> event.Score)
                    match played with
                    | Some played -> sourcedLogger.Debug("...{player} plays {previous}{played} = {runningTotal} -> scores {score}", (toPlayerDetails player).Name, previous, played, runningTotal, score)
                    | None -> sourcedLogger.Debug("...{player} claims a go -> scores {score}", (toPlayerDetails player).Name, score)
                    h :: t |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
                    peggingScoreEvent.Trigger(player, card, pegged, h :: t)
                    Some score
                | [] ->
                    match played with
                    | Some played -> sourcedLogger.Debug("...{player} plays {previous}{played} = {runningTotal}", (toPlayerDetails player).Name, previous, played, runningTotal)
                    | None -> raise NoScoreForGoException
                    None
            let peggingState = peggingState.Play(player, card, score)
            let hand = if player = Player1 then peggingState.Player1Hand else peggingState.Player2Hand
            if hand.Count > 0 then sourcedLogger.Debug("...{player} now has {cards}", (toPlayerDetails player).Name, cardsText hand)
            peggingState
        | None -> raise (UnexpectedPegException player)
    let dealSummary (currentDeal:CurrentDeal) =
        let peggingScore player =
            match currentDeal.PeggingState with
            | Some peggingState ->
                let score, hand = if player = Player1 then peggingState.Player1Score, peggingState.Player1Hand else peggingState.Player2Score, peggingState.Player2Hand
                Some (score, hand.Count = 0)
            | None -> None
        let player1WasDealer = currentDeal.IsDealer(Player1)
        let player1DealSummary = {
            WasDealer = player1WasDealer
            NibsScore = if player1WasDealer then currentDeal.NibsScore else None
            PeggingScore = peggingScore Player1
            HandScore = if player1WasDealer then currentDeal.DealerHandScore else currentDeal.NonDealerHandScore
            CribScore = if player1WasDealer then currentDeal.CribScore else None }
        let player2DealSummary = {
            WasDealer = not player1WasDealer
            NibsScore = if not player1WasDealer then currentDeal.NibsScore else None
            PeggingScore = peggingScore Player2
            HandScore = if not player1WasDealer then currentDeal.DealerHandScore else currentDeal.NonDealerHandScore
            CribScore = if not player1WasDealer then currentDeal.CribScore else None }
        {
            Player1DealSummary = player1DealSummary
            Player2DealSummary = player2DealSummary
        }
    let agent = MailboxProcessor<_>.Start(fun inbox ->
        let rec loop (initialInput:Input option) = async {
            match initialInput with
            | Some (ForCribNonInteractive player) ->
                match handleForCribNonInteractive player with
                | Some input -> inbox.Post input
                | None -> ()
                return! loop None
            | Some _ -> raise UnexpectedInitialInputException
            | None ->
                match awaitingForCribPlayer1.Value with
                | Some _ -> sourcedLogger.Debug("Awaiting cards for crib from {player}...", player1.Name)
                | None -> awaitingForCribInteractive Player1
                match awaitingForCribPlayer2.Value with
                | Some _ -> sourcedLogger.Debug("Awaiting cards for crib from {player}...", player2.Name)
                | None -> awaitingForCribInteractive Player2
                match awaitingPegPlayer1.Value with
                | Some pegState -> sourcedLogger.Debug("Awaiting {cardOrGo} from {player}...", (if pegState.Peggable.Count > 0 then "pegging card" else "go"), player1.Name)
                | None -> ()
                match awaitingPegPlayer2.Value with
                | Some pegState -> sourcedLogger.Debug("Awaiting {cardOrGo} from {player}...", (if pegState.Peggable.Count > 0 then "pegging card" else "go"), player2.Name)
                | None -> ()
                if awaitingCannotPegPlayer1.Value then sourcedLogger.Debug("Awaiting cannot peg from {player}...", player1.Name)
                if awaitingCannotPegPlayer2.Value then sourcedLogger.Debug("Awaiting cannot peg from {player}...", player2.Name)
                if awaitingNewDealPlayer1.Value then sourcedLogger.Debug("Awaiting new deal request from {player}...", player1.Name)
                if awaitingNewDealPlayer2.Value then sourcedLogger.Debug("Awaiting new deal request from {player}...", player2.Name)
                if awaitingNewGamePlayer1.Value then sourcedLogger.Debug("Awaiting new game request from {player}...", player1.Name)
                if awaitingNewGamePlayer2.Value then sourcedLogger.Debug("Awaiting new game request from {player}...", player2.Name)
                match! inbox.Receive() with
                | Interactive (ForCrib (player, forCrib)) ->
                    if (if player = Player1 then awaitingForCribPlayer1 else awaitingForCribPlayer2).Value |> Option.isNone then raise (UnexpectedForCribInteractiveException player)
                    let currentDeal = gameState.Value.CurrentDeal
                    let hand = handToCrib currentDeal player forCrib
                    transact (fun _ ->
                        gameState.Value <- { gameState.Value with CurrentDeal = currentDeal.UpdateHand(player, hand, forCrib) }
                        (if player = Player1 then awaitingForCribPlayer1 else awaitingForCribPlayer2).Value <- None)
                    if gameState.Value.ReadyForCut then inbox.Post Cut
                    return! loop None
                | Interactive (Peg (player, card)) ->
                    if (if player = Player1 then awaitingPegPlayer1 else awaitingPegPlayer2).Value |> Option.isNone then raise (UnexpectedPegInteractiveException player)
                    let currentDeal = gameState.Value.CurrentDeal
                    transact (fun _ ->
                        gameState.Value <- { gameState.Value with CurrentDeal = { currentDeal with PeggingState = Some (peg currentDeal.PeggingState player card) } }
                        (if player = Player1 then awaitingPegPlayer1 else awaitingPegPlayer2).Value <- None)
                    if gameState.Value.GameOver then inbox.Post ProcessGame
                    else inbox.Post Pegging
                    return! loop None
                | Interactive (CannotPeg player) ->
                    if not (if player = Player1 then awaitingCannotPegPlayer1 else awaitingCannotPegPlayer2).Value then raise (UnexpectedCannotPegInteractiveException player)
                    let currentDeal = gameState.Value.CurrentDeal
                    match currentDeal.PeggingState with
                    | Some peggingState ->
                        sourcedLogger.Debug("...{player} cannot peg", (toPlayerDetails player).Name)
                        let peggingState = if player = Player1 then { peggingState with Player1Knocked = true } else { peggingState with Player2Knocked = true }
                        transact (fun _ ->
                            gameState.Value <- { gameState.Value with CurrentDeal = { currentDeal with PeggingState = Some peggingState} }
                            (if player = Player1 then awaitingCannotPegPlayer1 else awaitingCannotPegPlayer2).Value <- false)
                        inbox.Post Pegging
                        return! loop None
                    | None -> raise (UnexpectedCannotPegInteractiveException player)
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
                    match handleForCribNonInteractive player with
                    | Some input -> inbox.Post input
                    | None -> ()
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
                        let currentDeal = gameState.Value.CurrentDeal
                        let peggingState = {
                            Player1Hand = currentDeal.Hand(Player1)
                            Player2Hand = currentDeal.Hand(Player2)
                            Player1Knocked = false
                            Player2Knocked = false
                            Player1Score = 0<point>
                            Player2Score = 0<point>
                            CurrentPegging = []
                            PreviousPegging = [] }
                        transact (fun _ -> gameState.Value <- { gameState.Value with CurrentDeal = { currentDeal with PeggingState = Some peggingState } })
                        inbox.Post Pegging
                    return! loop None
                | Pegging ->
                    let currentDeal = gameState.Value.CurrentDeal
                    match currentDeal.PeggingState with
                    | Some peggingState ->
                        if peggingState.Completed then
                            sourcedLogger.Debug("Pegging: {player} scored {score}", player1.Name, peggingState.Player1Score)
                            sourcedLogger.Debug("Pegging: {player} scored {score}", player2.Name, peggingState.Player2Score)
                            inbox.Post ScoreNonDealerHand
                        else
                            let toAct, canClaimGo, clearCurrent =
                                let currentIsMax, currentIsEmpty = peggingState.CurrentPips = MAX_PEGGING, peggingState.CurrentPegging.Length = 0
                                match peggingState.Player1Knocked, peggingState.Player2Knocked with
                                | true, true ->
                                    let toAct =
                                        match peggingState.CurrentPegging with
                                        | (_, player) :: _ -> otherPlayer player
                                        | [] -> raise InvalidCurrentPeggingException
                                    toAct, false, true
                                | true, false -> (if currentIsMax then Player1 else Player2), not (currentIsMax || currentIsEmpty), currentIsMax
                                | false, true -> (if currentIsMax then Player2 else Player1), not (currentIsMax || currentIsEmpty), currentIsMax
                                | false, false ->
                                    let toAct =
                                        match peggingState.CurrentPegging with
                                        | (_, player) :: _ -> otherPlayer player
                                        | [] -> otherPlayer currentDeal.Dealer
                                    toAct, false, currentIsMax
                            let peggingState =
                                if clearCurrent then
                                    { peggingState with
                                        Player1Knocked = false
                                        Player2Knocked = false
                                        CurrentPegging = []
                                        PreviousPegging = peggingState.CurrentPegging :: peggingState.PreviousPegging }
                                else peggingState
                            let peggingState, awaiting =
                                match toAct with
                                | Player1 when peggingState.Player1Hand.Count = 0 && not peggingState.Player2Knocked ->
                                    sourcedLogger.Debug("...{player} has no more cards to peg", player1.Name)
                                    { peggingState with Player1Knocked = true }, None
                                | Player2 when peggingState.Player2Hand.Count = 0 && not peggingState.Player1Knocked ->
                                    sourcedLogger.Debug("...{player} has no more cards to peg", player2.Name)
                                    { peggingState with Player2Knocked = true }, None
                                | player ->
                                    let hand = if player = Player1 then peggingState.Player1Hand else peggingState.Player2Hand
                                    if hand.Count = 0 then sourcedLogger.Debug("...{player} has no more cards to peg", (toPlayerDetails player).Name)
                                    let cutCard, selfCrib = currentDeal.ForPegState(player)
                                    let pegState = peggingState.PegState(player, cutCard, selfCrib, currentDeal.IsDealer(player))
                                    let peggable = pegState.Peggable
                                    let canPeg, isInteractive = peggable.Count > 0 || canClaimGo, if player = Player1 then player1IsInteractive else player2IsInteractive
                                    match isInteractive, canPeg with
                                    | true, true ->
                                        peggingState, Some (fun () -> (if player = Player1 then awaitingPegPlayer1 else awaitingPegPlayer2).Value <- Some pegState)
                                    | true, false -> peggingState, Some (fun () -> (if player = Player1 then awaitingCannotPegPlayer1 else awaitingCannotPegPlayer2).Value <- true)
                                    | false, true ->
                                        match toPlayerDetails player with
                                        | Human _ -> raise (UnexpectedPeggingForHumanPlayerException player)
                                        | Computer (name, _, pegStrategy) ->
                                            sourcedLogger.Debug("Choosing {cardOrGo} from {player}...", (if peggable.Count > 0 then "pegging card" else "go"), name)
                                            let card = pegStrategy pegState
                                            peg (Some peggingState) player card, None
                                    | false, false ->
                                        match toPlayerDetails player with
                                        | Human _ -> raise (UnexpectedPeggingForHumanPlayerException player)
                                        | Computer (name, _, pegStrategy) ->
                                            sourcedLogger.Debug("...{player} cannot peg", name)
                                            (if player = Player1 then { peggingState with Player1Knocked = true } else { peggingState with Player2Knocked = true }), None
                            transact (fun _ ->
                                gameState.Value <- { gameState.Value with CurrentDeal = { currentDeal with PeggingState = Some peggingState } }
                                match awaiting with | Some awaiting -> awaiting () | None -> ())
                            if gameState.Value.GameOver then inbox.Post ProcessGame
                            else if awaiting |> Option.isNone then inbox.Post Pegging
                        return! loop None
                    | None -> raise UnexpectedPeggingException
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
                            let crib = addToCrib (Set.empty, currentDeal.DealerForCrib)
                            let crib = addToCrib (crib, currentDeal.NonDealerForCrib)
                            let dealer = currentDeal.Dealer
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
#if FABLE_COMPILER
#else
    do agent.Error.Add (fun exn -> sourcedLogger.Error("Unexpected error -> {message}", exn.Message))
#endif
    member _.Players = player1, player2
    member _.Scores = gameState |> AVal.map (fun gameState -> gameState.Scores)
    member _.Dealer = gameState |> AVal.map (fun gameState -> gameState.CurrentDeal.Dealer)
    member _.Crib = gameState |> AVal.map (fun gameState ->
        let dealerForCrib, nonDealerForCrib = gameState.CurrentDeal.DealerForCrib, gameState.CurrentDeal.NonDealerForCrib
        match dealerForCrib.Count, nonDealerForCrib.Count with
        | 0, 0 -> None
        | 0, _ -> Some nonDealerForCrib
        | _, 0 -> Some dealerForCrib
        | _ ->
            let crib = addToCrib (Set.empty, dealerForCrib)
            Some (addToCrib (crib, nonDealerForCrib)))
    member _.CutCard = gameState |> AVal.map (fun gameState -> gameState.CurrentDeal.CutCard)
    member _.AwaitingForCrib(player) =
        (if player = Player1 then awaitingForCribPlayer1 else awaitingForCribPlayer2)
        |> AVal.map (fun awaiting ->
            match awaiting with
            | Some (isDealer, hand) -> Some (isDealer, hand, fun forCrib -> agent.Post(Interactive (ForCrib (player, forCrib))))
            | None -> None)
    member _.AwaitingPeg(player) =
        (if player = Player1 then awaitingPegPlayer1 else awaitingPegPlayer2)
        |> AVal.map (fun awaiting ->
            match awaiting with
            | Some pegState -> Some (pegState, fun card -> agent.Post(Interactive (Peg (player, card))))
            | None -> None)
    member _.AwaitingCannotPeg(player) =
        (if player = Player1 then awaitingCannotPegPlayer1 else awaitingCannotPegPlayer2)
        |> AVal.map (fun awaiting -> if awaiting then Some (fun () -> agent.Post(Interactive (CannotPeg player))) else None)
    member _.AwaitingNewDeal(player) =
        (if player = Player1 then awaitingNewDealPlayer1 else awaitingNewDealPlayer2)
        |> AVal.map (fun awaiting -> if awaiting then Some (fun () -> agent.Post(Interactive (RequestNewDeal player))) else None)
    member _.AwaitingNewGame(player) =
        (if player = Player1 then awaitingNewGamePlayer1 else awaitingNewGamePlayer2)
        |> AVal.map (fun awaiting -> if awaiting then Some (fun () -> agent.Post(Interactive (RequestNewGame player))) else None)
    member _.Quit(player) = agent.Post(Interactive (Quit player))
    member _.NibsScoreEvent = nibsScoreEvent.Publish
    member _.PeggingScoreEvent = peggingScoreEvent.Publish
    member _.HandScoreEvent = handScoreEvent.Publish
    member _.CribScoreEvent = cribScoreEvent.Publish
    member _.GameOverEvent = gameOverEvent.Publish
