module Aornota.Cribbage.HeuristicsConsole.Program

open Aornota.Cribbage.Common.Console
open Aornota.Cribbage.Common.IfDebug
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System

let [<Literal>] private SOURCE = "HeuristicsConsole.Program"

let private configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json", false)
#if DEBUG
        .AddJsonFile("appsettings.development.json", false)
#else
        .AddJsonFile("appsettings.production.json", false)
#endif
        .Build()

do Log.Logger <- LoggerConfiguration().ReadFrom.Configuration(configuration).Destructure.FSharpTypes().CreateLogger()

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let private mainAsync argv = async {
    writeNewLine "Running " ConsoleColor.Green
    write (ifDebug "Debug" "Release") ConsoleColor.DarkGreen
    write (sprintf " %s.mainAsync" SOURCE) ConsoleColor.Green
    write (sprintf " %A" argv) ConsoleColor.DarkGreen
    write "...\n\n" ConsoleColor.Green

    let mutable retval = 0

    try let deck = shuffledDeck()
        sourcedLogger.Debug("Shuffled deck: {deck}", deckText deck)
        let deck, dealt1 = dealToHand 6 (deck, Set.empty)
        let choice1 = randomChoice 2 dealt1
        let hand1, crib = removeFromHand (dealt1, choice1), addToCrib (Set.empty, choice1)
        sourcedLogger.Debug("Dealt 1: {dealt1} -> crib: {choice1}", cardsText dealt1, cardsText choice1)
        let deck, dealt2 = dealToHand 6 (deck, Set.empty)
        let choice2 = randomChoice 2 dealt2
        let hand2, crib = removeFromHand (dealt2, choice2), addToCrib (crib, choice2)
        sourcedLogger.Debug("Dealt 2: {dealt2} -> crib: {choice2}", cardsText dealt2, cardsText choice2)
        let cut = cut deck
        let nibsEvent = match NibsScoreEvent.Process cut with | Some event -> sprintf " -> %s" event.Text | None -> String.Empty
        sourcedLogger.Debug("Cut: {cut}{nibsEvent}", cardText cut, nibsEvent)
        // TODO-NMB: Pegging...
        let hand1Events = HandScoreEvent.Process(hand1, cut)
        sourcedLogger.Debug("Hand 1: {hand1} ({cut}) -> {score}", cardsText hand1, cardText cut, hand1Events |> List.sumBy (fun event -> event.Score))
        hand1Events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
        let hand2Events = HandScoreEvent.Process(hand2, cut)
        sourcedLogger.Debug("Hand 2: {hand2} ({cut}) -> {score}", cardsText hand2, cardText cut, hand2Events |> List.sumBy (fun event -> event.Score))
        hand2Events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
        let cribEvents = CribScoreEvent.Process(crib, cut)
        sourcedLogger.Debug("Crib: {crib} ({cut}) -> {score}", cardsText crib, cardText cut, cribEvents |> List.sumBy (fun event -> event.Score))
        cribEvents |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text))
    with | exn ->
        sourcedLogger.Error("Unexpected error:\n\t{errorMessage}", exn.Message)
        retval <- 1

    writeNewLine "Press any key to exit..." ConsoleColor.Green
    Console.ReadKey () |> ignore
    writeBlankLine ()
    return retval }

[<EntryPoint>]
let main argv =
    async {
        do! Async.SwitchToThreadPool()
        return! mainAsync argv
    } |> Async.RunSynchronously
