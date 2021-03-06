﻿module Aornota.Cribbage.Tests.Program

open Aornota.Cribbage.Common.Console
open Aornota.Cribbage.Common.IfDebug
open Aornota.Cribbage.Common.SourcedLogger

open Expecto
open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System

let [<Literal>] private SOURCE = "Tests.Program"

let private configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json", false)
        .Build()

do Log.Logger <- LoggerConfiguration().ReadFrom.Configuration(configuration).Destructure.FSharpTypes().CreateLogger()

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let private mainAsync argv = async {
    writeNewLine "Running " ConsoleColor.Magenta
    write (ifDebug "Debug" "Release") ConsoleColor.DarkMagenta
    write (sprintf " %s.mainAsync" SOURCE) ConsoleColor.Magenta
    write (sprintf " %A" argv) ConsoleColor.DarkMagenta
    write "...\n" ConsoleColor.Magenta

    let mutable retval = 0

    let updateRetval fRunTests =
        let retval' = fRunTests ()
        retval <- max retval retval'

    try writeNewLine "commonMathyTests:\n" ConsoleColor.DarkYellow
        updateRetval (fun _ -> runTestsWithCLIArgs [] [||] Tests.commonMathyTests)

        writeNewLine "domainCoreTests:\n" ConsoleColor.DarkYellow
        updateRetval (fun _ -> runTestsWithCLIArgs [] [||] Tests.domainCoreTests)

        writeNewLine "domainScoringTests:\n" ConsoleColor.DarkYellow
        updateRetval (fun _ -> runTestsWithCLIArgs [] [||] Tests.domainScoringTests)

        (* TEMP-NMB: These take ca. 5+ minutes for 100,000 iterations...
        if retval = 0 then // only run if no failures above.
            writeNewLine "domainScoringAdditionalTests:\n" ConsoleColor.DarkYellow
            updateRetval (fun _ -> runTestsWithCLIArgs [] [||] Tests.domainScoringAdditionalTests) *)

        if retval = 1 then
            writeBlankLine()
            sourcedLogger.Error "One or more tests failed"
    with | exn ->
        sourcedLogger.Error ("Unexpected error: {errorMessage}\n{stackTrace}", exn.Message, exn.StackTrace)
        retval <- 1

    writeBlankLine()
    return retval }

[<EntryPoint>]
let main argv =
    async {
        do! Async.SwitchToThreadPool()
        return! mainAsync argv
    } |> Async.RunSynchronously
