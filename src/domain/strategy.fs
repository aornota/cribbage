module Aornota.Cribbage.Domain.Strategy

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring

let forCribRandom (_, dealt:Hand) : CardS = randomChoice 2 dealt

let forCribBasic (_, dealt:Hand) : CardS =
    let cuts = deckExcept dealt
    let combo, _ =
        combinations [] 2 (dealt |> List.ofSeq)
        |> Seq.map (fun combo -> combo, combo |> Set.ofList |> Set.difference dealt)
        |> Seq.maxBy (fun (_, hand) -> cuts |> Seq.averageBy (fun cut -> HandScoreEvent.Process(hand, cut) |> List.sumBy (fun event -> event.Score) |> float))
    combo |> Set.ofList

let pegRandom (cards:CardL, hand:Hand) : Card option = if hand.Count = 0 then None else Some (randomSingle hand)

// TODO-NMB: pegBasic | ...
