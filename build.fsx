#r "paket: groupref build //"
#if !FAKE
// See https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095.
#r "netstandard"
#r "Facades/netstandard"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open System

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO

let private heuristicsConsoleDir = Path.getFullName "./src/heuristics-console"

let private testsDir = Path.getFullName "./src/tests"

let private runDotNet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd String.Empty
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s." cmd workingDir

let private createMissingAppSettingsForDevelopment dir =
    let requiredSettings, productionSettings = Path.combine dir "appsettings.development.json", "appsettings.production.json"
    if not (File.exists requiredSettings) then
        Shell.copyFile requiredSettings (Path.combine dir productionSettings)
        Trace.traceImportant (sprintf "WARNING -> %s did not exist and has been copied from %s; it will most likely need to be modified" requiredSettings productionSettings)

Target.create "run-heuristics-console" (fun _ ->
    createMissingAppSettingsForDevelopment heuristicsConsoleDir
    runDotNet "run" heuristicsConsoleDir)

Target.create "run-tests" (fun _ -> runDotNet "run -c Release" testsDir)

Target.create "help" (fun _ ->
    printfn "\nThese useful build targets can be run via 'fake build -t {target}':"
    printfn "\n\trun-heuristics-console -> builds and runs [Debug] heuristics-console"
    printfn "\n\trun-tests -> builds and runs [Release] tests"
    printfn "\n\thelp -> shows this list of build targets\n")

// TODO-NMB: Reinstante?..."run-tests" ==> "run-heuristics-console"

Target.runOrDefaultWithArguments "run-heuristics-console"
