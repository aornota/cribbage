module Aornota.Cribbage.Domain.Strategy

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring

type IsDealer = bool
type Peggable = CardS

// TODO-NMB: For advanced strategy, augment with subset of "game state" (e.g. scores | pegging "history")?...
type ForCribStrategy = IsDealer * Hand -> CardS
type PegStrategy = Pegged * Peggable -> Card option

let forCribRandom (_:IsDealer, dealt:Hand) : CardS = randomChoice 2 dealt

let forCribBasic (_:IsDealer, dealt:Hand) : CardS = // chooses 2-card combination with highest mean hand score for all possible cut cards
    let cuts = deckExcept dealt
    let combo, _ =
        combinations [] 2 (dealt |> List.ofSeq)
        |> Seq.map (fun combo -> combo, combo |> Set.ofList |> Set.difference dealt)
        |> Seq.maxBy (fun (_, hand) -> cuts |> Seq.averageBy (fun cut -> HandScoreEvent.Process(hand, cut) |> List.sumBy (fun event -> event.Score) |> float))
    combo |> Set.ofList

let pegRandom (_:Pegged, peggable:Peggable) : Card option = if peggable.Count = 0 then None else Some (randomSingle peggable)

let pegBasic (pegged:Pegged, peggable:Peggable) : Card option = // chooses highest-scoring card, else "safest", else random
    match peggable |> List.ofSeq |> List.choose (fun card -> match PeggingScoreEvent.Play(pegged, Some card) with | h :: t -> Some(card, h :: t) | [] -> None) with
    | h :: t ->
        let (card, _) = h :: t |> List.maxBy (fun (_, events) -> events |> List.sumBy (fun event -> event.Score))
        Some card
    | [] ->
        let isSafeZone pips = pips < 5<pip> || (pips > 15<pip> && pips < 21<pip>)
        match peggable |> List.ofSeq |> List.choose (fun card -> if isSafeZone (pips (card :: pegged)) then Some card else None) with
        | h :: t -> Some (randomSingle (h :: t |> Set.ofList))
        | [] -> pegRandom (pegged, peggable)
