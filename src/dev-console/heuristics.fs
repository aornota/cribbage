[<RequireQualifiedAccess>]
module Aornota.Cribbage.DevConsole.Heuristics

open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Strategy

open Serilog
open System
open System.IO

let [<Literal>] private SOURCE = "DevConsole.Heuristics"

let [<Literal>] private DOT_EVERY = 50

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let private heuristics (strategy:ForCribStrategy) =
    let deck = shuffledDeck ()
    sourcedLogger.Debug("Shuffled deck: {deck}", deckText deck)
    let _, dealt = dealToHand 6 (deck, Set.empty)
    sourcedLogger.Debug("Dealt: {dealt}", cardsText dealt)
    let forCribDealer = strategy (true, dealt)
    let handDealer = removeFromHand (dealt, forCribDealer)
    sourcedLogger.Debug("\twhen dealer -> hand: {handDealer} | for crib: {forCribDealer}", cardsText handDealer, cardsText forCribDealer)
    let forCribNonDealer = strategy (false, dealt)
    let handNonDealer = removeFromHand (dealt, forCribNonDealer)
    sourcedLogger.Debug("\twhen not dealer -> hand: {handNonDealer} | for crib: {forCribNonDealer}", cardsText handNonDealer, cardsText forCribNonDealer)
    handDealer, forCribDealer, handNonDealer, forCribNonDealer

let private zeroMap () = [ King ; Queen ; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two ; Ace ] |> List.map (fun rank -> rank, 0) |> Map.ofList

let private updateMap map (cards:CardS) =
    cards |> Set.fold (fun (state:Map<Rank, int>) (rank, _) -> match state.TryGetValue(rank) with | true, value -> state.Add(rank, value + 1) | false, _ -> failwithf "Rank %A not in map" rank) map

let private writeFile outputDir fileName (map:Map<Rank, int>) =
    let total = float (map |> Map.toList |> List.sumBy snd)
    let lines = map |> Map.toList |> List.map (fun (rank, count) -> sprintf "%s -> %.4f" rank.FullText (float count / total))
    File.WriteAllLines(Path.Combine(outputDir, fileName), lines)

let run srcDir suffix strategy n =
    if n <= 0 then failwithf "%s must be greater than zero" (nameof n)
    sourcedLogger.Information("Running '{suffix}' heuristics for {n} deal/s...\n", suffix, n)
    let mutable handDealerMap, forCribDealerMap, handNonDealerMap, forCribNonDealerMap = zeroMap (), zeroMap (), zeroMap (), zeroMap ()
    for i in 1 .. n do
        if i % DOT_EVERY = 0 then Console.Write(".")
        let handDealer, forCribDealer, handNonDealer, forCribNonDealer = heuristics strategy
        handDealerMap <- updateMap handDealerMap handDealer
        forCribDealerMap <- updateMap forCribDealerMap forCribDealer
        handNonDealerMap <- updateMap handNonDealerMap handNonDealer
        forCribNonDealerMap <- updateMap forCribNonDealerMap forCribNonDealer
    if n >= DOT_EVERY then Console.WriteLine("\n")
    let outputDir = Path.Combine(srcDir, "heuristics")
    if not (Directory.Exists(outputDir)) then Directory.CreateDirectory(outputDir) |> ignore
    let fileName = sprintf "handDealer-%s-%i.txt" suffix n
    writeFile outputDir fileName handDealerMap
    let fileName = sprintf "forCribDealer-%s-%i.txt" suffix n
    writeFile outputDir fileName forCribDealerMap
    let fileName = sprintf "handNonDealer-%s-%i.txt" suffix n
    writeFile outputDir fileName handNonDealerMap
    let fileName = sprintf "forCribNonDealer-%s-%i.txt" suffix n
    writeFile outputDir fileName forCribNonDealerMap
    sourcedLogger.Information("...heuristics written to {outputDir}", outputDir)
