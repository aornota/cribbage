[<RequireQualifiedAccess>]
module Aornota.Cribbage.DevConsole.GamePlayer

open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.State

open FSharp.Data.Adaptive
open Serilog

let [<Literal>] private SOURCE = "DevConsole.GamePlayer"

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let play (player1:Player) (player2:Player) (bestOf:byte) = async {
    if player1.IsInteractive then failwithf "%s must not be Interactive" (nameof player1)
    if player2.IsInteractive then failwithf "%s must not be Interactive" (nameof player2)
    if bestOf = 0uy then failwithf "%s must be greater than zero" (nameof bestOf)
    sourcedLogger.Debug("{name1} vs. {name2} (best of {bestOf} games)...", player1.Name, player2.Name, bestOf)
    let state = State(player1, player2, Some Log.Logger)
    let mutable loop = true
    while loop do
        sourcedLogger.Debug "Waiting for current game to finish..."
        do! Async.Sleep 1000
        if AVal.force state.CurrentGameIsFinished then
            if int (AVal.force state.Player1Games) + int (AVal.force state.Player2Games) < int bestOf then state.NextGame()
            else loop <- false }
