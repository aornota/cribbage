[<RequireQualifiedAccess>]
module Aornota.Cribbage.DevConsole.Heuristics

open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring
open Aornota.Cribbage.Domain.Strategy

open Serilog
open System

let [<Literal>] private SOURCE = "DevConsole.Heuristics"

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let workInProgress () =
    // TODO-NMB: use forCribIntermediate to work out hand-/crib-rank-frequency heuristics (dealer | non-dealer)?...

    let deck = shuffledDeck ()
    sourcedLogger.Debug("Shuffled deck: {deck}", deckText deck)
    let _, dealt = dealToHand 6 (deck, Set.empty)
    sourcedLogger.Debug("Dealt: {dealt}", cardsText dealt)
    let forCribDealer = forCribIntermediate (true, dealt)
    let handDealer = removeFromHand (dealt, forCribDealer)
    sourcedLogger.Debug("\twhen dealer -> hand: {handDealer} | for crib: {forCribDealer}", cardsText handDealer, cardsText forCribDealer)
    let forCribNonDealer = forCribIntermediate (false, dealt)
    let handNonDealer = removeFromHand (dealt, forCribNonDealer)
    sourcedLogger.Debug("\twhen not dealer -> hand: {handNonDealer} | for crib: {forCribNonDealer}", cardsText handNonDealer, cardsText forCribNonDealer)

    () // TEMP-NMB...
