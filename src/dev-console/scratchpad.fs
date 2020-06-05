[<RequireQualifiedAccess>]
module Aornota.Cribbage.DevConsole.Scratchpad

open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring
open Aornota.Cribbage.Domain.Strategy

open Serilog
open System

let [<Literal>] private SOURCE = "DevConsole.Scratchpad"

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let workInProgress () =
    let deck = shuffledDeck ()
    sourcedLogger.Debug("Shuffled deck: {deck}", deckText deck)
    let deck, dealt1 = dealToHand 6 (deck, Set.empty)
    let forCrib1 = forCribBasic (true, dealt1)
    let hand1, crib = removeFromHand (dealt1, forCrib1), addToCrib (Set.empty, forCrib1)
    sourcedLogger.Debug("Dealt 1: {dealt1} -> crib: {forCrib1}", cardsText dealt1, cardsText forCrib1)
    let deck, dealt2 = dealToHand 6 (deck, Set.empty)
    let forCrib1 = forCribBasic (false, dealt2)
    let hand2, crib = removeFromHand (dealt2, forCrib1), addToCrib (crib, forCrib1)
    sourcedLogger.Debug("Dealt 2: {dealt2} -> crib: {forCrib1}", cardsText dealt2, cardsText forCrib1)
    let cut = cut deck
    let nibsEvent = match NibsScoreEvent.Process cut with | Some event -> sprintf " -> %s" event.Text | None -> String.Empty
    sourcedLogger.Debug("Cut: {cut}{nibsEvent}", cardText cut, nibsEvent)
    let hand1Events = HandScoreEvent.Process(hand1, cut)
    sourcedLogger.Debug("Hand 1: {hand1} | {cut} -> {score}", cardsText hand1, cardText cut, hand1Events |> List.sumBy (fun event -> event.Score))
    hand1Events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
    let hand2Events = HandScoreEvent.Process(hand2, cut)
    sourcedLogger.Debug("Hand 2: {hand2} | {cut} -> {score}", cardsText hand2, cardText cut, hand2Events |> List.sumBy (fun event -> event.Score))
    hand2Events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
    let cribEvents = CribScoreEvent.Process(crib, cut)
    sourcedLogger.Debug("Crib: {crib} | {cut} -> {score}", cardsText crib, cardText cut, cribEvents |> List.sumBy (fun event -> event.Score))
    cribEvents |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
