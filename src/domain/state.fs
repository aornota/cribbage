module Aornota.Cribbage.Domain.State

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring

open FSharp.Data.Adaptive
open Serilog
open System

type PlayerId = | PlayerId of Guid with static member Create() = PlayerId (Guid.NewGuid())

exception PlayersHaveSameNameException
exception UnknownPlayerIdException of PlayerId
exception NoCurrentDealerException
exception PlayerIsNotInteractiveException
exception CurrentDealStillInProgressException
exception HandDoesNotContain6CardsException
exception CurrentGameStillInProgressException

type private Logger (sourcedLogger:ILogger option) =
    member _.Debug(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) = match sourcedLogger with | Some sourcedLogger -> sourcedLogger.Debug(messageTemplate, propertyValues) | None -> ()

type IsDealer = bool
type Pegged = CardL
type Peggable = CardS

// TODO-NMB: Augment with subset of "game state" (e.g. scores / pegging "history")?...
type ForCribStrategy = IsDealer * Hand -> CardS
type PegStrategy = Pegged * Peggable -> Card option

type Player =
    | Human of PlayerId * string
    | Computer of PlayerId * string * ForCribStrategy * PegStrategy
    with
    member this.IsInteractive = match this with | Human _ -> true | Computer _ -> false
    member this.Name = match this with | Human (_, name) | Computer (_, name, _, _) -> name
    member this.PlayerId = match this with | Human (playerId, _) | Computer (playerId, _, _, _) -> playerId

let [<Literal>] private SOURCE = "Domain.State"

let [<Literal>] private GAME_TARGET = 121<point>

(* TODO-NMB:
     - remember to toggle firstDealer (and set dealer back to None) before starting a new game...
     - ... *)

type State (player1:Player, player2:Player, logger:ILogger option) =
    do if player1.Name = player2.Name then raise PlayersHaveSameNameException
    let sourcedLogger, logger = Logger(logger |> Option.map (sourcedLogger SOURCE)), ()
    do sourcedLogger.Debug("Initializing for {name1} vs. {name2}...", player1.Name, player2.Name)
    let games1, games2 = cval 0<game>, cval 0<game> // TODO-NMB: Store game "history" instead (and tweak a-values accordingly)?...
    let score1, score2 = cval 0<point>, cval 0<point> // TODO-NMB: Store deal "history" instead (&c.)?...
    let firstDealer = cval (if normalizedRandom () < 0.5 then player1.PlayerId else player2.PlayerId)
    let dealer : cval<PlayerId option> = cval None
    let deck : cval<Deck> = cval []
    let hand1 : cval<Hand> = cval Set.empty
    let hand2 : cval<Hand> = cval Set.empty
    let crib : cval<Crib> = cval Set.empty
    let cut : cval<Card option> = cval None
    let player playerId = if playerId = player1.PlayerId then player1 else if playerId = player2.PlayerId then player2 else raise (UnknownPlayerIdException playerId)
    let hand playerId = if playerId = player1.PlayerId then hand1 else if playerId = player2.PlayerId then hand2 else raise (UnknownPlayerIdException playerId)
    let isDealer playerId = match dealer.Value with | Some dealer -> dealer = playerId | None -> raise NoCurrentDealerException
    let failIfNot6Cards (hand:cval<Hand>) = if hand.Value.Count <> 6 then raise HandDoesNotContain6CardsException
    let failIfNotInteractive playerId = if not (player playerId).IsInteractive then raise PlayerIsNotInteractiveException
    let deal () =
        if hand1.Value.Count > 0 || hand2.Value.Count > 0 then raise CurrentDealStillInProgressException
        let newDealer =
            match dealer.Value with
            | Some playerId -> if playerId = player1.PlayerId then player2.PlayerId else player1.PlayerId
            | None -> firstDealer.Value
        sourcedLogger.Debug("Dealing hands ({name} is dealer)...", (player newDealer).Name)
        let newDeck = shuffledDeck ()
        let newDeck, dealt1 = dealToHand 6 (newDeck, Set.empty)
        let newDeck, dealt2 = dealToHand 6 (newDeck, Set.empty)
        sourcedLogger.Debug("...dealt to {name1} -> {dealt1}", player1.Name, cardsText dealt1)
        sourcedLogger.Debug("...dealt to {name2} -> {dealt2}", player2.Name, cardsText dealt2)
        transact (fun _ ->
            dealer.Value <- Some newDealer
            deck.Value <- newDeck
            hand1.Value <- dealt1
            hand2.Value <- dealt2
            crib.Value <- Set.empty)
    let handToCrib (player:Player) forCrib =
        let hand = hand player.PlayerId
        failIfNot6Cards hand
        let newHand, newCrib = removeFromHand (hand.Value, forCrib), addToCrib (crib.Value, forCrib)
        sourcedLogger.Debug("...{name} adds {forCrib} to crib -> hand is {newHand}", player.Name, cardsText forCrib, cardsText newHand)
        transact (fun _ ->
            hand.Value <- newHand
            crib.Value <- newCrib)
    let forCrib (player:Player) =
        let hand = hand player.PlayerId
        failIfNot6Cards hand
        match player with
        | Human _ -> sourcedLogger.Debug("Awaiting cards for crib from {name}...", player.Name)
        | Computer (playerId, _, forCribStrategy, _) ->
            sourcedLogger.Debug("Choosing cards for crib from {name}...", player.Name)
            handToCrib player (forCribStrategy (isDealer playerId, hand.Value))

    let currentGameIsFinished = adaptive {
        let! score1 = score1
        let! score2 = score2
        return score1 >= GAME_TARGET || score2 >= GAME_TARGET }

    let nextGame () =
        // TODO-NMB: Figure this out properly...if not (AVal.force currentGameIsFinished) then raise CurrentGameStillInProgressException
        transact (fun _ ->
            score1.Value <- 0<point>
            score2.Value <- 0<point>
            firstDealer.Value <- if firstDealer.Value = player1.PlayerId then player2.PlayerId else player1.PlayerId
            dealer.Value <- None
            deck.Value <- []
            hand1.Value <- Set.empty
            hand2.Value <- Set.empty
            crib.Value <- Set.empty
            cut.Value <- None)
        deal ()
        forCrib player1
        forCrib player2
        // TODO-NMB: How to handle awaiting interactive?... let resume (after:unit -> unit)...
        // TEMP-NMB...
        transact (fun _ ->
            score1.Value <- 121<point>
            score2.Value <- 108<point>
            games1.Value <- games1.Value + 1<game>
            games2.Value <- games2.Value)

    do nextGame ()

    member _.Player1Games = games1
    member _.Player2Games = games2
    member _.Player1Score = score1
    member _.Player2Score = score2
    member _.Player1Hand = hand1
    member _.Player2Hand = hand2
    member _.Cut = cut
    member _.CurrentGameIsFinished = currentGameIsFinished
    member _.ForCrib(playerId, forCrib) =
        failIfNotInteractive playerId
        handToCrib (player playerId) forCrib
    member _.NextGame() = nextGame ()
