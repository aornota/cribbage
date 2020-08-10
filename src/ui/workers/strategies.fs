[<RequireQualifiedAccessAttribute>]
module Aornota.Cribbage.Ui.Workers.Strategies

open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Strategy

open Feliz.UseWorker

let [<Literal>] private UMD_PATH = "strategies"

// Note: Set<Card> caused de/serialization issues - so map this to list<card>.

let forCribRandomWorker = WorkerFunc.Create (UMD_PATH, "forCribRandomWorker", fun (isDealer, hand) -> forCribRandom (isDealer, hand |> Set.ofList) |> List.ofSeq)
let forCribBasicWorker = WorkerFunc.Create (UMD_PATH, "forCribBasicWorker", fun (isDealer, hand) -> forCribBasic (isDealer, hand |> Set.ofList) |> List.ofSeq)
let forCribIntermediateWorker = WorkerFunc.Create (UMD_PATH, "forCribIntermediateWorker", fun (isDealer, hand) -> forCribIntermediate (isDealer, hand |> Set.ofList) |> List.ofSeq)

let private fromAnon (anon:{| previouslyPegged : Pegged list ; pegged : Pegged ; peggable : Card list ; notPeggable : Card list ; cutCard : Card ; selfCrib : Card list ; isDealer : IsDealer |}) = {
    PreviouslyPegged = anon.previouslyPegged
    Pegged = anon.pegged
    Peggable = anon.peggable |> Set.ofList
    NotPeggable = anon.notPeggable |> Set.ofList
    CutCard = anon.cutCard
    SelfCrib = anon.selfCrib |> Set.ofList
    IsDealer = anon.isDealer }

// Note: option<Card> also problematic - so hack around this.

let private mungeOption = function | Some card -> true, card | None -> false, (Ace, Spade) // relies on consuming code ignoring Card when false

let pegRandomWorker = WorkerFunc.Create (UMD_PATH, "pegRandomWorker", fromAnon >> pegRandom >> mungeOption)
let pegBasicWorker = WorkerFunc.Create (UMD_PATH, "pegBasicWorker", fromAnon >> pegBasic >> mungeOption)
let pegIntermediateWorker = WorkerFunc.Create (UMD_PATH, "pegIntermediateWorker", fromAnon >> pegIntermediate >> mungeOption)
