module Aornota.Cribbage.DevConsole.Program

open Aornota.Cribbage.Common.Console
open Aornota.Cribbage.Common.IfDebug
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Strategy

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.IO

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

let rec private findSrcDir (currentDir:DirectoryInfo) = if currentDir.Name = "src" then currentDir.FullName else findSrcDir currentDir.Parent

let private mainAsync argv = async {
    writeNewLine "Running " ConsoleColor.Green
    write (ifDebug "Debug" "Release") ConsoleColor.DarkGreen
    write (sprintf " %s.mainAsync" SOURCE) ConsoleColor.Green
    write (sprintf " %A" argv) ConsoleColor.DarkGreen
    write "...\n\n" ConsoleColor.Green

    let mutable retval = 0

    try (* TEMP-NMB...
        do! GamePlayer.computerVsComputer GamePlayer.intermediate GamePlayer.basic 1 *)
        (* TEMP-NMB...
        do! GamePlayer.humanVsComputer GamePlayer.neph GamePlayer.random 1 *)
        (* TEMP-NMB... *)
        do! GamePlayer.humanVsHuman GamePlayer.neph GamePlayer.jack 1

        (* TEMP-NMB...
        Heuristics.run (findSrcDir (DirectoryInfo(Environment.CurrentDirectory))) "intermediate" forCribIntermediate 50000 *)

        ()
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
