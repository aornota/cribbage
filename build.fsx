#r "paket: groupref build //"
#if !FAKE
// See https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095.
#r "netstandard"
#r "Facades/netstandard"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

#nowarn "52"

open System

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools.Git

let private uiDir = Path.getFullName "./src/ui"
let private uiPublishDir = uiDir </> "publish"

let private devConsoleDir = Path.getFullName "./src/dev-console"

let private testsDir = Path.getFullName "./src/tests"

let private platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | None -> failwithf "%s not found in path. Please install it and make sure it's available from your path. See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info." tool

let private yarnTool = platformTool "yarn" "yarn.cmd"

let private runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand(cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let private runDotNet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd String.Empty
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s." cmd workingDir

let private openBrowser url =
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "Unable to open browser."
    |> Proc.run
    |> ignore

let private createMissingAppSettingsForDevelopment dir =
    let requiredSettings, productionSettings = Path.combine dir "appsettings.development.json", "appsettings.production.json"
    if not (File.exists requiredSettings) then
        Shell.copyFile requiredSettings (Path.combine dir productionSettings)
        Trace.traceImportant (sprintf "WARNING -> %s did not exist and has been copied from %s; it will most likely need to be modified" requiredSettings productionSettings)

Target.create "clean-ui" (fun _ ->
    !! (uiDir </> "bin")
    ++ (uiDir </> "obj")
    ++ (uiDir </> "public/Workers")
    ++ uiPublishDir
    |> Seq.iter Shell.cleanDir)

Target.create "restore-ui" (fun _ ->
    printfn "Yarn version:"
    runTool yarnTool "--version" __SOURCE_DIRECTORY__
    runTool yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
    runDotNet "restore" uiDir)

Target.create "build-workers" (fun _ -> runTool yarnTool "webpack-cli --config src/ui/workers/webpack.config.js" __SOURCE_DIRECTORY__)

Target.create "run" (fun _ ->
    let ui = async { runTool yarnTool "webpack-dev-server" __SOURCE_DIRECTORY__ }
    let browser = async {
        do! Async.Sleep 2500
        openBrowser "http://localhost:8080" }
    Async.Parallel [ ui ; browser ] |> Async.RunSynchronously |> ignore)

Target.create "build" (fun _ -> runTool yarnTool "webpack-cli -p" __SOURCE_DIRECTORY__)

Target.create "publish-gh-pages" (fun _ ->
    let tempGhPagesDir = __SOURCE_DIRECTORY__ </> "temp-gh-pages"
    Shell.cleanDir tempGhPagesDir
    Repository.cloneSingleBranch "" "https://github.com/aornota/cribbage.git" "gh-pages" tempGhPagesDir
    Repository.fullclean tempGhPagesDir
    Shell.copyRecursive uiPublishDir tempGhPagesDir true |> Trace.logfn "%A"
    Staging.stageAll tempGhPagesDir
    Commit.exec tempGhPagesDir (sprintf "Publish gh-pages (%s)" (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")))
    Branches.push tempGhPagesDir)

Target.create "run-dev-console" (fun _ ->
    createMissingAppSettingsForDevelopment devConsoleDir
    runDotNet "run" devConsoleDir)

Target.create "run-tests" (fun _ -> runDotNet "run -c Release" testsDir)

Target.create "help" (fun _ ->
    printfn "\nThese useful build targets can be run via 'fake build -t {target}':"
    printfn "\n\trun -> builds, runs and watches [non-production] ui (served via webpack-dev-server)"
    printfn "\n\tbuild -> builds [production] ui (which writes output to .\\src\\ui\\publish)"
    printfn "\n\tpublish-gh-pages -> builds [production] ui, then pushes to gh-pages branch"
    printfn "\n\trun-dev-console -> builds and runs [Debug] dev-console"
    printfn "\n\trun-tests -> builds and runs [Release] tests"
    printfn "\n\thelp -> shows this list of build targets\n")

"clean-ui" ==> "restore-ui" ==> "build-workers"

"build-workers" ==> "run"
"build-workers" ==> "build" ==> "publish-gh-pages"

// TODO-NMB: Reinstate?..."run-tests" ==> "run-dev-console"

Target.runOrDefaultWithArguments "help"
