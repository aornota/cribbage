module Aornota.Cribbage.Domain.State

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Common.SourcedLogger
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring

open FSharp.Data.Adaptive
open Serilog

exception PlayersHaveSameNameException

type private Logger (sourcedLogger:ILogger option) =
    member _.Debug(messageTemplate: string, [<System.ParamArray>] propertyValues: obj []) = match sourcedLogger with | Some sourcedLogger -> sourcedLogger.Debug(messageTemplate, propertyValues) | None -> ()

type private Dealer = | Player1 | Player2

// TODO-NMB: Augment (e.g. with scores / pegging "history")?...
type ForCribStrategy = bool * Hand -> CardS
type PegStrategy = CardL * Hand -> Card option

type Player =
    | Human of string
    | Computer of string * ForCribStrategy * PegStrategy
    with
    member this.IsInteractive = match this with | Human _ -> true | Computer _ -> false
    member this.Name = match this with | Human name | Computer (name, _, _) -> name

let [<Literal>] private SOURCE = "Domain.State"

let [<Literal>] private GAME_TARGET = 121<point>

type State (player1:Player, player2:Player, logger:ILogger option) =
    do if player1.Name = player2.Name then raise PlayersHaveSameNameException
    let sourcedLogger, logger = Logger(logger |> Option.map (sourcedLogger SOURCE)), ()
    do sourcedLogger.Debug("Initializing for {name1} vs. {name2}...", player1.Name, player2.Name)
    let cGames1, cGames2 = cval 0<game>, cval 0<game> // TODO-NMB: Store game "history" instead (and use a-values for game scores)?...
    let cScore1, cScore2 = cval 0<point>, cval 0<point>
    let dealer = cval (if normalizedRandom () < 0.5 then Player1 else Player2)
    let deck : cval<Deck> = cval []
    let hand1 : cval<Hand> = cval Set.empty
    let hand2 : cval<Hand> = cval Set.empty
    let crib : cval<Crib> = cval Set.empty
    //let deck = cval (shuffledDeck ())
    //let updateDeck newDeck = transact (fun _ -> deck.Value <- newDeck)
    //let newDeck, dealt1 = dealToHand 6 (deck.Value, Set.empty)
    //do updateDeck newDeck
    //let forCrib1 = forCribBasic (true, dealt1)
    //let hand1, crib = removeFromHand (dealt1, forCrib1), addToCrib (Set.empty, forCrib1)



    member _.Player1Games = adaptive { return! cGames1 }
    member _.Player2Games = adaptive { return! cGames2 }
    member _.Player1Score = adaptive { return! cScore1 }
    member _.Player2Score = adaptive { return! cScore2 }
    member _.CurrentGameIsFinished = adaptive {
        let! score1 = cScore1
        let! score2 = cScore2
        return score1 > GAME_TARGET || score2 > GAME_TARGET }
    member _.NextGame() =
        () // TEMP-NMB...
