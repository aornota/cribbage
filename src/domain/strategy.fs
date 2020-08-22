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
    SelfCrib : CardS
    IsDealer : IsDealer }

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

// Chooses 2-card combination with highest mean hand score - optionally adjusted by "partial crib" score - for all possible cut cards.
let private forCrib adjustForPartialCribScore (isDealer:IsDealer, dealt:Hand) : CardS =
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

// Chooses highest-scoring card, else random "safe zone", else random "not danger zone", else random.
let pegBasic (pegState:PegState) =
    let peggable = pegState.Peggable
    match peggable.Count with
    | 0 | 1 -> pegNoneOnlyOrRandom peggable
    | _ ->
        let pegged = pegState.Pegged |> List.map fst
        let highestScoring =
            let scoring =
                peggable
                |> List.ofSeq
                |> List.choose (fun card -> match PeggingScoreEvent.Play(pegged, Some card) with | h :: t -> Some (card, h :: t |> List.sumBy (fun event -> event.Score)) | [] -> None)
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

// Chooses highest-scoring card (adjusted by mean "opponent next card" score).
// TODO-NMB: Further adjust, e.g. by "self next card" score [unless "opponent next" totals 31]?...
let pegIntermediate (pegState:PegState) =
    let previouslyPegged, pegged, peggable = pegState.PreviouslyPegged, pegState.Pegged, pegState.Peggable
    match peggable.Count with
    | 0 | 1 -> pegNoneOnlyOrRandom peggable
    | _ ->
        let allPegged = (previouslyPegged |> List.collect id) @ pegged
        let known = pegState.CutCard :: (allPegged |> List.map fst) @ (peggable |> List.ofSeq) @ (pegState.NotPeggable |> List.ofSeq) @ (pegState.SelfCrib |> List.ofSeq)
        let unknown = deckExcept (known |> Set.ofList)
        let opponentCouldHave =
            match previouslyPegged with
            | h :: t ->
                let highestTotal = h :: t |> List.map (fun pegged -> pegged |> List.map fst |> pips) |> List.max
                let lowestPossible = (MAX_PEGGING - highestTotal) + 1<pip>
                unknown |> Set.filter (fun (rank, _) -> rank.PipValue >= lowestPossible)
            | [] -> unknown
        let opponentHasCards = (allPegged |> List.filter (snd >> not)).Length < 4
        let opponentHasKnocked = match pegState.Pegged with | (_, true) :: _ -> true | _ -> false
        let pegged = pegged |> List.map fst
        let adjustedScores =
            peggable
            |> List.ofSeq
            |> List.map (fun card ->
                let selfScore = PeggingScoreEvent.Play(pegged, Some card) |> List.sumBy (fun event -> event.Score)
                let opponentNextScore =
                    if not opponentHasCards || opponentHasKnocked || pips (card :: pegged) = MAX_PEGGING then 0.0
                    else
                        let pegged = card :: pegged
                        let pips = pips pegged
                        opponentCouldHave
                        |> List.ofSeq
                        |> List.averageBy (fun (rank, suit) ->
                            if rank.PipValue + pips <= MAX_PEGGING then float (PeggingScoreEvent.Play(pegged, Some (rank, suit)) |> List.sumBy (fun event -> event.Score))
                            else 0.0)
                card, (float selfScore) - opponentNextScore)
        let (card, _) = adjustedScores |> List.maxBy snd
        Some card

// TODO-NMB: pegAdvanced, i.e. similar to pegIntermediate - but using opponent-hand-rank-frequency heuristics?...
