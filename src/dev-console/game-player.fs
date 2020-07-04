[<RequireQualifiedAccess>]
module Aornota.Cribbage.DevConsole.GamePlayer

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.GameEngine
open Aornota.Cribbage.Domain.Strategy

open FSharp.Data.Adaptive
open Serilog
open System

let [<Literal>] private SOURCE = "DevConsole.GamePlayer"

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let mutable private gameSummaries : GameSummary list = []
let mutable private hasQuit = false

let private validateGames games =
    if games <= 0 then failwithf "%s must be greater than zero" (nameof games)
    games * 1<game>

let private log (name1:string) (name2:string) (games:int<game>) = sourcedLogger.Information("{name1} vs. {name2} ({games} game/s)...", name1, name2, games)

let private statistics player =
    match gameSummaries with
    | h :: t ->
        let playerGameSummaries = h :: t |> List.map (fun summary -> (if player = Player1 then summary.Player1GameSummary else summary.Player2GameSummary), summary.IsWinner(player))
        let games = playerGameSummaries.Length * 1<game>
        let winPercentage = (float (playerGameSummaries |> List.filter (fun (_, won) -> won) |> List.length) / float games) * 100.0
        let playerGameSummaries = playerGameSummaries |> List.map fst
        let zero = { Total = 0<point> ; Count = 0 }
        let gameMean = Mean<_>.FromList(playerGameSummaries |> List.map (fun summary -> summary.Score))
        let peggingMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.PeggingMean)) zero
        let peggingDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.PeggingDealerMean)) zero
        let peggingNotDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.PeggingNotDealerMean)) zero
        let handMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.HandMean)) zero
        let handDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.HandDealerMean)) zero
        let handNotDealerMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.HandNotDealerMean)) zero
        let cribMean = playerGameSummaries |> List.fold (fun acc summary -> Mean<_>.Combine(acc, summary.CribMean)) zero
        Some (games, winPercentage, gameMean, peggingMean, peggingDealerMean, peggingNotDealerMean, handMean, handDealerMean, handNotDealerMean, cribMean)
    | [] -> None

let private logStatistics (name:string) (statistics:(int<game> * float * Mean<point> * Mean<point> * Mean<point> * Mean<point> * Mean<point> * Mean<point> * Mean<point> * Mean<point>) option) =
    match statistics with
    | Some (games, winPercentage, gameMean, peggingMean, peggingDealerMean, peggingNotDealerMean, handMean, handDealerMean, handNotDealerMean, cribMean) ->
        sourcedLogger.Information("Statistics for {name} ({games} game/s):", name, games)
        sourcedLogger.Information("\tWin percentage -> {percentage}%", Math.Round(winPercentage, 2))
        sourcedLogger.Information("\tMean game score -> {mean}", Math.Round(float gameMean.Mean, 2))
        sourcedLogger.Information("\tMean pegging score -> {mean}", Math.Round(float peggingMean.Mean, 2))
        sourcedLogger.Information("\t\twhen dealer -> {mean}", Math.Round(float peggingDealerMean.Mean, 2))
        sourcedLogger.Information("\t\twhen not dealer -> {mean}", Math.Round(float peggingNotDealerMean.Mean, 2))
        sourcedLogger.Information("\tMean hand score -> {mean}", Math.Round(float handMean.Mean, 2))
        sourcedLogger.Information("\t\twhen dealer -> {mean}", Math.Round(float handDealerMean.Mean, 2))
        sourcedLogger.Information("\t\twhen not dealer -> {mean}", Math.Round(float handNotDealerMean.Mean, 2))
        sourcedLogger.Information("\tMean crib score -> {mean}", Math.Round(float cribMean.Mean, 2))
    | None -> ()

let private handleScoreEvents (engine:GameEngine) (name1:string) (name2:string) =
    let name player = if player = Player1 then name1 else name2
    engine.NibsScoreEvent.Add(fun (player, cutCard, event) ->
        sourcedLogger.Information("Cut: {cutCard} -> {player} scores {event}", cardText cutCard, name player, event.Text))
    engine.PeggingScoreEvent.Add(fun (player, card, pegged, events) ->
        let played = match card with | Some card -> Some (cardText card, (fst card).PipValue) | None -> None
        let score = events |> List.sumBy (fun event -> event.Score)
        match played with
        | Some (played, pipValue) ->
            let runningTotal = pips pegged + pipValue
            let previous = match pegged |> List.map cardText with | h :: t -> sprintf "(%s) " (h :: t |> String.concat " ") | [] -> ""
            sourcedLogger.Information("Pegging: {player} plays {previous}{played} = {runningTotal} -> scores {score}", name player, previous, played, runningTotal, score)
        | None -> sourcedLogger.Debug("Pegging: {player} claims a go -> scores {score}", name player, score)
        events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text)))
    engine.HandScoreEvent.Add(fun (player, hand, cutCard, events) ->
        let score = events |> List.sumBy (fun event -> event.Score)
        sourcedLogger.Information("Hand: {hand} | {cutCard} -> {name} scores {score}", cardsText hand, cardText cutCard, name player, score)
        events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text)))
    engine.CribScoreEvent.Add(fun (player, crib, cutCard, events) ->
        let score = events |> List.sumBy (fun event -> event.Score)
        sourcedLogger.Information("Crib: {crib} | {cutCard} -> {name} scores {score}", cardsText crib, cardText cutCard, name player, score)
        events |> List.iter (fun event -> sourcedLogger.Debug("\t{event}", event.Text)))

let private handleGameOverEvent (engine:GameEngine) games (name1:string) (name2:string) =
    engine.GameOverEvent.Add(fun gameSummary ->
        gameSummaries <- gameSummary :: gameSummaries
        let player1Games = gameSummaries |> List.sumBy (fun gameSummary -> if gameSummary.IsWinner(Player1) then 1 else 0)
        let player2Games = gameSummaries |> List.sumBy (fun gameSummary -> if gameSummary.IsWinner(Player2) then 1 else 0)
        sourcedLogger.Information("...game finished -> {name1} ({player1Score}) {player1Games} - {player2Games} ({player2Score}) {name2}",
            name1, gameSummary.Player1Score, player1Games, player2Games, gameSummary.Player2Score, name2)
        if gameSummaries.Length = int games then
            engine.Quit(if player1Games > player2Games then Player2 else Player1)
            let dealsPerGameMean = Mean<_>.FromList(gameSummaries |> List.map (fun gameSummary -> gameSummary.Deals))
            sourcedLogger.Information("Deals per game ({games} game/s) -> {mean}", games, Math.Round(dealsPerGameMean.Mean, 2))
            logStatistics name1 (statistics Player1)
            logStatistics name2 (statistics Player2)
            hasQuit <- true)

let private scoresCallback (name1:string) (name2:string) (score1, score2) = if score1 + score2 > 0<point> then sourcedLogger.Debug("...{name1} {score1} - {score2} {name2}", name1, score1, score2, name2)

let private awaitingForCribCallback (strategy:ForCribStrategy) = function | Some (isDealer, hand, forCrib) -> forCrib (strategy (isDealer, hand)) | None -> ()

let private awaitingPegCallback (strategy:PegStrategy) = function | Some (pegState, peg) -> peg (strategy pegState) | None -> ()
let private awaitingCannotPegCallback = function | Some cannotPeg -> cannotPeg () | None -> ()

let private awaitingNewDealCallback = function | Some newDeal -> newDeal () | None -> ()
let private awaitingNewGameCallback = function | Some newGame -> (if not hasQuit then newGame () else ()) | None -> ()

// TODO-NMB: advancedStategy?...
let private intermediateStrategy : ForCribStrategy * PegStrategy = forCribIntermediate, pegIntermediate
let private basicStrategy : ForCribStrategy * PegStrategy = forCribBasic, pegBasic
let private randomStrategy : ForCribStrategy * PegStrategy = forCribRandom, pegRandom

let intermediate, basic, random = ("Intermediate", intermediateStrategy), ("Basic", basicStrategy), ("Random", randomStrategy)
let neph, jack = ("Neph", intermediateStrategy), ("Jack", basicStrategy)

let computerVsComputer (computer1, strategy1) (computer2, strategy2) games = async {
    let games = validateGames games
    let computer1, computer2 = if computer1 = computer2 then sprintf "%s %i" computer1 1, sprintf "%s %i" computer2 2 else computer1, computer2
    log computer1 computer2 games
    let engine = GameEngine(Computer (computer1, fst strategy1, snd strategy1), Computer (computer2, fst strategy2, snd strategy2))
    (* TEMP-NMB...
    handleScoreEvents engine computer1 computer2 *)
    handleGameOverEvent engine games computer1 computer2
    use scoresCallback = engine.Scores.AddCallback(scoresCallback computer1 computer2)
    while not hasQuit do do! Async.Sleep 250 }

let humanVsComputer (human, strategy1:ForCribStrategy * PegStrategy) (computer, strategy2) games = async {
    let games = validateGames games
    let human, computer = if human = computer then sprintf "%s %i" human 1, sprintf "%s %i" computer 2 else human, computer
    log human computer games
    let engine = GameEngine(Human human, Computer (computer, fst strategy2, snd strategy2))
    (* TEMP-NMB...
    handleScoreEvents engine human computer *)
    handleGameOverEvent engine games human computer
    use scoresCallback = engine.Scores.AddCallback(scoresCallback human computer)
    use awaitingForCrib1Callback = engine.AwaitingForCrib(Player1).AddCallback(awaitingForCribCallback (fst strategy1))
    use awaitingPeg1Callback = engine.AwaitingPeg(Player1).AddCallback(awaitingPegCallback (snd strategy1))
    use awaitingCannotPeg1Callback = engine.AwaitingCannotPeg(Player1).AddCallback(awaitingCannotPegCallback)
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player1).AddCallback(awaitingNewDealCallback)
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player1).AddCallback(awaitingNewGameCallback)
    while not hasQuit do do! Async.Sleep 250 }

let humanVsHuman (human1, strategy1:ForCribStrategy * PegStrategy) (human2, strategy2:ForCribStrategy * PegStrategy) games = async {
    let games = validateGames games
    let human1, human2 = if human1 = human2 then sprintf "%s %i" human1 1, sprintf "%s %i" human2 2 else human1, human2
    log human1 human2 games
    let engine = GameEngine(Human human1, Human human2)
    (* TEMP-NMB...
    handleScoreEvents engine human1 human2 *)
    handleGameOverEvent engine games human1 human2
    use scoresCallback = engine.Scores.AddCallback(scoresCallback human1 human2)
    use awaitingForCrib1Callback = engine.AwaitingForCrib(Player1).AddCallback(awaitingForCribCallback (fst strategy1))
    use awaitingPeg1Callback = engine.AwaitingPeg(Player1).AddCallback(awaitingPegCallback (snd strategy1))
    use awaitingCannotPeg1Callback = engine.AwaitingCannotPeg(Player1).AddCallback(awaitingCannotPegCallback)
    use awaitingNewDeal1Callback = engine.AwaitingNewDeal(Player1).AddCallback(awaitingNewDealCallback)
    use awaitingNewGame1Callback = engine.AwaitingNewGame(Player1).AddCallback(awaitingNewGameCallback)
    use awaitingForCrib2Callback = engine.AwaitingForCrib(Player2).AddCallback(awaitingForCribCallback (fst strategy2))
    use awaitingPeg2Callback = engine.AwaitingPeg(Player2).AddCallback(awaitingPegCallback (snd strategy2))
    use awaitingCannotPeg2Callback = engine.AwaitingCannotPeg(Player2).AddCallback(awaitingCannotPegCallback)
    use awaitingNewDeal2Callback = engine.AwaitingNewDeal(Player2).AddCallback(awaitingNewDealCallback)
    use awaitingNewGame2Callback = engine.AwaitingNewGame(Player2).AddCallback(awaitingNewGameCallback)
    while not hasQuit do do! Async.Sleep 250 }
