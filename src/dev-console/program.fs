module Aornota.Cribbage.DevConsole.Program

open Aornota.Cribbage.Common.Console
open Aornota.Cribbage.Common.IfDebug
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.State
open Aornota.Cribbage.Domain.Strategy

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System

let [<Literal>] private SOURCE = "DevConsole.Program"

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

    try do! GamePlayer.play (Computer (PlayerId.Create(), "Basic", forCribBasic, pegRandom)) (Computer (PlayerId.Create(), "Random", forCribRandom, pegRandom)) 3uy

        (* TEMP-NMB...
        Scratchpad.workInProgress () *)
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
