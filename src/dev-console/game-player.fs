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

// TODO-NMB: Add computer-vs.-human?  | human-vs.-human? | ...

let computerVsComputer games =
    if games <= 0 then failwithf "%s must be greater than zero" (nameof games)
    let games = games * 1<game>
    let name1, name2 = "Basic", "Random"
    sourcedLogger.Information("{name1} vs. {name2} ({games} game/s)...", name1, name2, games)
    let engine = Engine(Computer (name1, forCribBasic, pegBasic), Computer (name2, forCribRandom, pegRandom))
    let gamesCallback (games1, games2) =
        let total = games1 + games2
        if total > 0<game> then sourcedLogger.Information("...game finished -> {name1} {games1} - {games2} {name2}", name1, games1, games2, name2)
        // TODO-NMB: Check for specific key press - and quit?...
        if total >= games then
            // TODO-NMB: Output statistics, e.g. average pegging | hand | crib scores (when dealer | non-dealer) for each player?...
            let quitter = if games1 > games2 then Player2 else Player1
            engine.Quit(quitter)
    let scoresCallback (score1, score2) = sourcedLogger.Information("...{name1} {score1} - {score2} {name2}", name1, score1, score2, name2)
    use gamesCallback = engine.Games.AddCallback gamesCallback
    use scoresCallback = engine.Scores.AddCallback scoresCallback
    engine.Start()
