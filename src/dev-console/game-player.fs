[<RequireQualifiedAccess>]
module Aornota.Cribbage.DevConsole.GamePlayer

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Engine
open Aornota.Cribbage.Domain.Strategy

open FSharp.Data.Adaptive
open Serilog
open System

let [<Literal>] private SOURCE = "DevConsole.GamePlayer"

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let mutable private hasQuit = false

let private validateGames games =
    if games <= 0 then failwithf "%s must be greater than zero" (nameof games)
    games * 1<game>

let private log (name1:string) (name2:string) (games:int<game>) = sourcedLogger.Information("{name1} vs. {name2} ({games} game/s)...", name1, name2, games)

let private statistics (name:string) (statistics:(int<game> * Mean<point> * Mean<point> * Mean<point> * Mean<point> * Mean<point> * Mean<point> * Mean<point>) option) =
    match statistics with
    | Some (games, peggingMean, peggingDealerMean, peggingNotDealerMean, handMean, handDealerMean, handNotDealerMean, cribMean) ->
        sourcedLogger.Information("Statistics for {name} ({games} game/s):", name, games)
        (* TODO-NMB: Once non-zero...
        sourcedLogger.Information("\tMean pegging score -> {mean}", Math.Round(float peggingMean.Mean, 2))
        sourcedLogger.Debug("\t\twhen dealer -> {mean}", Math.Round(float peggingDealerMean.Mean, 2))
        sourcedLogger.Debug("\t\twhen not dealer -> {mean}", Math.Round(float peggingNotDealerMean.Mean, 2)) *)
        sourcedLogger.Information("\tMean hand score -> {mean}", Math.Round(float handMean.Mean, 2))
        sourcedLogger.Debug("\t\twhen dealer -> {mean}", Math.Round(float handDealerMean.Mean, 2))
        sourcedLogger.Debug("\t\twhen not dealer -> {mean}", Math.Round(float handNotDealerMean.Mean, 2))
        sourcedLogger.Information("\tMean crib score -> {mean}", Math.Round(float cribMean.Mean, 2))
    | None -> ()

let private gamesCallback (name1:string) (name2:string) games (engine:Engine) (games1, games2) =
    let total = games1 + games2
    if total > 0<game> then sourcedLogger.Information("...game finished -> {name1} {games1} - {games2} {name2}", name1, games1, games2, name2)
    if total >= games then
        engine.Quit(if games1 > games2 then Player2 else Player1)
        hasQuit <- true
        statistics name1 (engine.Statistics(Player1) |> AVal.force)
        statistics name2 (engine.Statistics(Player2) |> AVal.force)

let private scoresCallback (name1:string) (name2:string) (score1, score2) = if score1 + score2 > 0<point> then sourcedLogger.Debug("...{name1} {score1} - {score2} {name2}", name1, score1, score2, name2)

let private awaitingForCribCallback strategy = function | Some (isDealer, hand, forCrib) -> forCrib (strategy (isDealer, hand)) | None -> ()

let private awaitingPegCallback strategy = function | Some (pegged, peggable, peg) -> peg (strategy (pegged, peggable)) | None -> ()
let private awaitingCannotPegCallback = function | Some cannotPeg -> cannotPeg () | None -> ()

let private awaitingNewDealCallback = function | Some newDeal -> newDeal () | None -> ()
let private awaitingNewGameCallback = function
    | Some newGame -> if not hasQuit then newGame () else ()
    | None -> ()

let basicStrategy : ForCribStrategy * PegStrategy = forCribBasic, pegBasic
let randomStrategy : ForCribStrategy * PegStrategy = forCribRandom, pegRandom

let computerVsComputer (computer1, strategy1) (computer2, strategy2) games =
    let games = validateGames games
    let computer1, computer2 = if computer1 = computer2 then sprintf "%s %i" computer1 1, sprintf "%s %i" computer2 2 else computer1, computer2
    log computer1 computer2 games
    let engine = Engine(Computer (computer1, fst strategy1, snd strategy1), Computer (computer2, fst strategy2, snd strategy2))
    use gamesCallback = engine.Games.AddCallback (gamesCallback computer1 computer2 games engine)
    use scoresCallback = engine.Scores.AddCallback (scoresCallback computer1 computer2)
    engine.Start()

let humanVsComputer (human, strategy1:ForCribStrategy * PegStrategy) (computer, strategy2) games =
    let games = validateGames games
    log human computer games
    let engine = Engine(Human human, Computer (computer, fst strategy2, snd strategy2))
    use gamesCallback = engine.Games.AddCallback (gamesCallback human computer games engine)
    use scoresCallback = engine.Scores.AddCallback (scoresCallback human computer)
    use awaitingForCrib1Callback = engine.AwaitingForCrib(Player1).AddCallback (awaitingForCribCallback (fst strategy1))
    // TODO-NMB...use awaitingPeg1Callback = engine.AwaitingPeg(Player1).AddCallback (awaitingPegCallback (snd strategy1))
    // TODO-NMB...use awaitingCannotPeg1Callback = engine.AwaitingCannotPeg(Player1).AddCallback awaitingCannotPegCallback
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player1).AddCallback awaitingNewDealCallback
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player1).AddCallback awaitingNewGameCallback
    engine.Start()

let humanVsHuman (human1, strategy1:ForCribStrategy * PegStrategy) (human2, strategy2:ForCribStrategy * PegStrategy) games =
    let games = validateGames games
    log human1 human2 games
    let engine = Engine(Human human1, Human human2)
    use gamesCallback = engine.Games.AddCallback (gamesCallback human1 human2 games engine)
    use scoresCallback = engine.Scores.AddCallback (scoresCallback human1 human2)
    use awaitingForCrib1Callback = engine.AwaitingForCrib(Player1).AddCallback (awaitingForCribCallback (fst strategy1))
    use awaitingForCrib2Callback = engine.AwaitingForCrib(Player2).AddCallback (awaitingForCribCallback (fst strategy2))
    // TODO-NMB...use awaitingPeg1Callback = engine.AwaitingPeg(Player1).AddCallback (awaitingPegCallback (snd strategy1))
    // TODO-NMB...use awaitingCannotPeg1Callback = engine.AwaitingCannotPeg(Player1).AddCallback awaitingCannotPegCallback
    // TODO-NMB...use awaitingPeg2Callback = engine.AwaitingPeg(Player2).AddCallback (awaitingPegCallback (snd strategy2))
    // TODO-NMB...use awaitingCannotPeg2Callback = engine.AwaitingCannotPeg(Player2).AddCallback awaitingCannotPegCallback
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player1).AddCallback awaitingNewDealCallback
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player2).AddCallback awaitingNewDealCallback
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player1).AddCallback awaitingNewGameCallback
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player2).AddCallback awaitingNewGameCallback
    engine.Start()
