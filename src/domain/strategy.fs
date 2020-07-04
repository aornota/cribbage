module Aornota.Cribbage.Domain.Strategy

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring

exception PartialCribDoesNotContain2CardsException of string
exception PartialCribMustNotContainCutCardException of string

type IsDealer = bool

type ForCribStrategy = IsDealer * Hand -> CardS

type IsSelf = bool
type Pegged = (Card * IsSelf) list

type PegState = {
    PreviouslyPegged : Pegged list
    Pegged : Pegged
    Peggable : CardS
    NotPeggable : CardS
    CutCard : Card
    SelfCrib : CardS }

type PegStrategy = PegState -> Card option

let private partialCribScore (isDealer:IsDealer) (partialCrib:CardL) (cutCard:Card) =
    if partialCrib.Length <> 2 then raise (PartialCribDoesNotContain2CardsException (sprintf "Partial Crib (%s) does not contain 2 Cards" (cardsText (partialCrib |> Set.ofList))))
    else if partialCrib |> List.contains cutCard then
        raise (PartialCribMustNotContainCutCardException (sprintf "Partial Crib (%s) must not contain cut Card (%s)" (cardsText (partialCrib |> Set.ofList)) (cardText cutCard)))
    let all = cutCard :: partialCrib
    let distinctRanks = all |> List.map fst |> List.distinct |> List.length
    let fifteensScore = (fifteens [] [ all ]).Length * 2
    let pairsScore = match distinctRanks with | 1 -> 6 | 2 -> 2 | _ -> 0
    let runScore = if distinctRanks = 3 then match isRun all with | Some _ -> 3 | None -> 0 else 0
    (fifteensScore + pairsScore + runScore) * (if isDealer then 1<point> else -1<point>)

let private forCrib adjustForPartialCribScore (isDealer:IsDealer, dealt:Hand) : CardS = // chooses 2-card combination with highest mean hand score (adjusted by "partial crib" score) for all possible cut cards
    let cutCards = deckExcept dealt
    let combo, _ =
        combinations [] 2 (dealt |> List.ofSeq)
        |> Seq.map (fun combo -> combo, combo |> Set.ofList |> Set.difference dealt)
        |> Seq.maxBy (fun (combo, hand) ->
            cutCards
            |> Seq.averageBy (fun cutCard ->
                let handScore = HandScoreEvent.Process(hand, cutCard) |> List.sumBy (fun event -> event.Score)
                float (if adjustForPartialCribScore then handScore + (partialCribScore isDealer combo cutCard) else handScore)))
    combo |> Set.ofList

let forCribRandom (_:IsDealer, dealt:Hand) = randomChoice 2 dealt

let forCribBasic (isDealer:IsDealer, dealt:Hand) : CardS = forCrib false (isDealer, dealt)

let forCribIntermediate (isDealer:IsDealer, dealt:Hand) : CardS = forCrib true (isDealer, dealt)

// TODO-NMB: forCribAdvanced, i.e. similar to forCribIntermediate - but using opponent-crib-rank-frequency heuristics and "full crib" scoring (but non-exhaustive sampling)?...

let private pegNoneOnlyOrRandom (peggable:CardS) = if peggable.Count = 0 then None else Some (randomSingle peggable)

let pegRandom (pegState:PegState) = pegNoneOnlyOrRandom pegState.Peggable

let pegBasic (pegState:PegState) = // chooses highest-scoring card, else random "safe zone", else random "not danger zone", else random
    let pegged = pegState.Pegged |> List.map fst
    let peggable = pegState.Peggable
    match peggable.Count with
    | 0 | 1 -> pegNoneOnlyOrRandom peggable
    | _ ->
        let highestScoring =
            let scoring =
                peggable
                |> List.ofSeq
                |> List.choose (fun card -> match PeggingScoreEvent.Play(pegged, Some card) with | h :: t -> Some(card, h :: t |> List.sumBy (fun event -> event.Score)) | [] -> None)
            match scoring with
            | h :: t ->
                let max = h :: t |> List.map snd |> List.max
                h :: t |> List.choose (fun (card, score) -> if score = max then Some card else None)
            | [] -> []
        match highestScoring with
        | h :: t -> Some (randomSingle (h :: t |> Set.ofList))
        | [] ->
            let isSafeZone pips = pips < 5<pip> || (pips > 15<pip> && pips < 21<pip>)
            match peggable |> List.ofSeq |> List.choose (fun card -> if isSafeZone (pips (card :: pegged)) then Some card else None) with
            | h :: t -> Some (randomSingle (h :: t |> Set.ofList))
            | [] ->
                let isDangerZone pips = pips = 5<pip> || pips = 21<pip>
                match peggable |> List.ofSeq |> List.choose (fun card -> if isDangerZone (pips (card :: pegged)) then None else Some card) with
                | h :: t -> Some (randomSingle (h :: t |> Set.ofList))
                | [] -> pegNoneOnlyOrRandom peggable

// TODO-NMB: pegIntermediate, i.e. similar to pegBasic - but adjusted for oppenent-next-peg score?...

// TODO-NMB: pegAdvanced, i.e. similar to pegIntermediate - but using opponent-hand-rank-frequency heuristics?...
