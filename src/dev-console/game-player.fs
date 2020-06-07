[<RequireQualifiedAccess>]
module Aornota.Cribbage.DevConsole.GamePlayer

open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Engine
open Aornota.Cribbage.Domain.Strategy

open FSharp.Data.Adaptive
open Serilog

let [<Literal>] private SOURCE = "DevConsole.GamePlayer"

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let private validateGames games =
    if games <= 0 then failwithf "%s must be greater than zero" (nameof games)
    games * 1<game>

let private log (name1:string) (name2:string) (games:int<game>) = sourcedLogger.Information("{name1} vs. {name2} ({games} game/s)...", name1, name2, games)

let private gamesCallback (name1:string) (name2:string) games (engine:Engine) (games1, games2) =
    let total = games1 + games2
    if total > 0<game> then sourcedLogger.Information("...game finished -> {name1} {games1} - {games2} {name2}", name1, games1, games2, name2)
    if total >= games then
        // TODO-NMB: Output statistics, e.g. average pegging | hand | crib scores (when dealer | non-dealer) for each player?...
        let quitter = if games1 > games2 then Player2 else Player1
        engine.Quit(quitter)

let private scoresCallback (name1:string) (name2:string) (score1, score2) = if score1 + score2 > 0<point> then sourcedLogger.Debug("...{name1} {score1} - {score2} {name2}", name1, score1, score2, name2)

let private awaitingForCribCallback strategy = function | Some (isDealer, hand, forCrib) -> forCrib (strategy (isDealer, hand)) | None -> ()

let private awaitingPegCallback strategy = function | Some (pegged, peggable, peg) -> peg (strategy (pegged, peggable)) | None -> ()
let private awaitingCannotPegCallback = function | Some cannotPeg -> cannotPeg () | None -> ()

let private awaitingNewDealCallback = function | Some newDeal -> newDeal () | None -> ()
let private awaitingNewGameCallback = function | Some newGame -> newGame () | None -> ()

let computerVsComputer games =
    let games = validateGames games
    //let name1, name2 = "Basic 1", "Basic 2"
    let name1, name2 = "Basic", "Random"
    //let name1, name2 = "Random 1", "Random 2"
    log name1 name2 games
    //let engine = Engine(Computer (name1, forCribBasic, pegBasic), Computer (name2, forCribBasic, pegBasic))
    let engine = Engine(Computer (name1, forCribBasic, pegBasic), Computer (name2, forCribRandom, pegRandom))
    //let engine = Engine(Computer (name1, forCribRandom, pegRandom), Computer (name2, forCribRandom, pegRandom))
    use gamesCallback = engine.Games.AddCallback (gamesCallback name1 name2 games engine)
    use scoresCallback = engine.Scores.AddCallback (scoresCallback name1 name2)
    engine.Start()

let humanVsComputer games =
    let games = validateGames games
    let name1, name2 = "Neph", "Random"
    log name1 name2 games
    let engine = Engine(Human name1, Computer (name2, forCribRandom, pegRandom))
    use gamesCallback = engine.Games.AddCallback (gamesCallback name1 name2 games engine)
    use scoresCallback = engine.Scores.AddCallback (scoresCallback name1 name2)
    use awaitingForCrib1Callback = engine.AwaitingForCrib(Player1).AddCallback (awaitingForCribCallback forCribBasic)
    // TODO-NMB...use awaitingPeg1Callback = engine.AwaitingPeg(Player1).AddCallback (awaitingPegCallback pegBasic)
    // TODO-NMB...use awaitingCannotPeg1Callback = engine.AwaitingCannotPeg(Player1).AddCallback awaitingCannotPegCallback
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player1).AddCallback awaitingNewDealCallback
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player1).AddCallback awaitingNewGameCallback
    engine.Start()

let humanVsHuman games =
    let games = validateGames games
    let name1, name2 = "Neph", "Jack"
    log name1 name2 games
    let engine = Engine(Human name1, Human name2)
    use gamesCallback = engine.Games.AddCallback (gamesCallback name1 name2 games engine)
    use scoresCallback = engine.Scores.AddCallback (scoresCallback name1 name2)
    use awaitingForCrib1Callback = engine.AwaitingForCrib(Player1).AddCallback (awaitingForCribCallback forCribBasic)
    use awaitingForCrib2Callback = engine.AwaitingForCrib(Player2).AddCallback (awaitingForCribCallback forCribBasic)
    // TODO-NMB...use awaitingPeg1Callback = engine.AwaitingPeg(Player1).AddCallback (awaitingPegCallback pegBasic)
    // TODO-NMB...use awaitingCannotPeg1Callback = engine.AwaitingCannotPeg(Player1).AddCallback awaitingCannotPegCallback
    // TODO-NMB...use awaitingPeg2Callback = engine.AwaitingPeg(Player2).AddCallback (awaitingPegCallback pegBasic)
    // TODO-NMB...use awaitingCannotPeg2Callback = engine.AwaitingCannotPeg(Player2).AddCallback awaitingCannotPegCallback
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player1).AddCallback awaitingNewDealCallback
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player2).AddCallback awaitingNewDealCallback
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player1).AddCallback awaitingNewGameCallback
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player2).AddCallback awaitingNewGameCallback
    engine.Start()
