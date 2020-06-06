module Aornota.Cribbage.Domain.Scoring

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Domain.Core

exception CannotPlayNoneException of string
exception CardAlreadyPlayedException of string
exception CannotPlayCardException of string
exception DoesNotContain4CardsException of string
exception MustNotContainCutCardException of string

type Pegged = CardL

let private isRun (cards:CardL) =
    match cards.Length with
    | len when len > 2 ->
        let ranks = cards |> List.map fst
        if ranks |> List.distinct |> List.length = len then
            let max, min = ranks |> List.maxBy (fun rank -> rank.Value), ranks |> List.minBy (fun rank -> rank.Value)
            if (max.Value - min.Value) + 1 = len then Some (max, min)
            else None
        else None
    | _ -> None

type PeggingScoreEvent =
    | PeggingPair of Rank
    | ThreeOfAKind of Rank
    | FourOfAKind of Rank
    | PeggingRun of high:Rank * low:Rank
    | PeggingFifteen
    | ThirtyOne
    | Go
    with
    static member Play (pegged:Pegged, card:Card option) : PeggingScoreEvent list =
        match card, pegged with
        | None, [] -> raise (CannotPlayNoneException "Cannot play None when no Cards pegged")
        | None, _ -> [ Go ]
        | Some _, [] -> []
        | Some card, _ ->
            if pegged |> List.contains card then raise (CardAlreadyPlayedException (sprintf "%s has already been played" (cardText card)))
            let rank, _ = card
            let runningTotal = pips pegged + rank.PipValue
            if runningTotal > MAX_PEGGING then raise (CannotPlayCardException (sprintf "Cannot play %s when running total is %i" (cardText card) (int runningTotal)))
            let rec findRun (cards:CardL) =
                match cards.Length with
                | len when len > 3 ->
                    match isRun cards with
                    | Some (high, low) -> Some (high, low)
                    | None -> findRun (cards |> List.take (len - 1))
                | 3 -> isRun cards
                | _ -> None
            [
                match pegged with
                | h :: t when fst h = rank ->
                    match t with
                    | h :: t when fst h = rank ->
                        match t with
                        | h :: _ when fst h = rank -> FourOfAKind rank
                        | _ -> ThreeOfAKind rank
                    | _ -> PeggingPair rank
                | _ ->
                    match findRun (card :: pegged) with
                    | Some (high, low) -> PeggingRun (high, low)
                    | None -> ()
                if runningTotal = 15<pip> then PeggingFifteen
                else if runningTotal = MAX_PEGGING then ThirtyOne
            ]
    member this.Score =
        (match this with | PeggingPair _ | PeggingFifteen | ThirtyOne -> 2 | ThreeOfAKind _ -> 6 | FourOfAKind _ -> 12 | PeggingRun (r1, r2) -> (r1.Value - r2.Value) + 1 | Go -> 1) * 1<point>
    member this.Text =
        match this with
        | PeggingPair rank -> sprintf "Pair of %ss for %i" rank.FullText this.Score
        | ThreeOfAKind rank -> sprintf "Three %ss for %i" rank.FullText this.Score
        | FourOfAKind rank -> sprintf "Four %ss for %i" rank.FullText this.Score
        | PeggingRun (high, low) -> sprintf "Run from %s to %s for %i" high.FullText low.FullText this.Score
        | PeggingFifteen -> sprintf "Fifteen for %i" this.Score
        | ThirtyOne -> sprintf "Thirty-one for %i" this.Score
        | Go -> sprintf "Go for %i" this.Score

let rec private fifteens acc (cardsL:CardL list) =
    cardsL
    |> List.collect (fun cards ->
        match cards.Length with
        | 0 | 1 -> acc
        | len ->
            match pips cards with
            | 15<pip> -> (cards |> Set.ofList) :: acc
            | n when n < 15<pip> -> acc
            | _ -> fifteens acc (combinations [] (len  - 1) cards |> List.ofSeq))
    |> List.distinct

let private runs (cards:CardL) =
    let rec runs acc (cardsL:CardL list) =
        cardsL
        |> List.collect (fun cards ->
            match cards.Length with
            | 0 | 1 | 2 -> acc
            | len ->
                match isRun cards with
                | Some _ -> (cards |> Set.ofList) :: acc
                | None -> runs acc (combinations [] (len  - 1) cards |> List.ofSeq))
        |> List.distinct
    let runs = runs [] [ cards ]
    runs |> List.filter (fun cards -> not (runs |> List.exists (fun otherCards -> otherCards |> Set.isProperSubset cards)))

type CommonScoreEvent =
    | Fifteen of CardS
    | Pair of CardS
    | Run of CardS
    | FiveFlush of CardS
    | Nob of Card
    with
    static member Process (isCrib, cards:CardS, cut:Card) : CommonScoreEvent list =
        if cards.Count <> 4 then raise (DoesNotContain4CardsException (sprintf "%s (%s) does not contain 4 Cards" (if isCrib then "Crib" else "Hand") (cardsText cards)))
        else if cards.Contains cut then raise (MustNotContainCutCardException (sprintf "%s (%s) must not contain cut Card (%s)" (if isCrib then "Crib" else "Hand") (cardsText cards) (cardText cut)))
        let all = cut :: (cards |> List.ofSeq)
        let distinctRanks = all |> List.map fst |> List.distinct |> List.length
        [
            yield! fifteens [] [ all ] |> List.map Fifteen
            if distinctRanks < 5 then
                yield! all
                |> List.groupBy fst
                |> List.map snd
                |> List.filter (fun cards -> cards.Length > 1)
                |> List.collect (fun cards -> combinations [] 2 cards |> List.ofSeq)
                |> List.map (Set.ofList >> Pair)
            if distinctRanks > 2 then
                yield! runs all |> List.map Run
            if distinctRanks = 5 then
                match all |> List.groupBy snd |> List.map fst with
                | [ _ ] -> FiveFlush (all |> Set.ofList)
                | _ -> ()
            if fst cut <> Jack then
                let nob = Jack, snd cut
                if cards.Contains nob then Nob nob
        ]
    member this.Score = (match this with | Fifteen _ | Pair _ -> 2 | Run cards -> cards.Count | FiveFlush _ -> 5 | Nob _ -> 1) * 1<point>
    member this.Text =
        match this with
        | Fifteen cards -> sprintf "Fifteen (%s) for %i" (cardsText cards) this.Score
        | Pair cards -> sprintf "Pair (%s) for %i" (cardsText cards) this.Score
        | Run cards -> sprintf "Run (%s) for %i" (cardsText cards) this.Score
        | FiveFlush cards -> sprintf "Flush (%s) for %i" (cardsText cards) this.Score
        | Nob card -> sprintf "%i for his nob (%s)" this.Score (cardText card)
and CribScoreEvent =
    | CribScoreEvent of CommonScoreEvent
    with
    static member Process (crib:Crib, cut:Card) : CribScoreEvent list = CommonScoreEvent.Process (true, crib, cut) |> List.map CribScoreEvent
    member this.Score = match this with | CribScoreEvent event -> event.Score
    member this.Text = match this with | CribScoreEvent event -> event.Text

type HandScoreEvent =
    | CommonScoreEvent of CommonScoreEvent
    | FourFlush of CardS
    with
    static member Process (hand:Hand, cut:Card) : HandScoreEvent list =
        [
            yield! CommonScoreEvent.Process (false, hand, cut) |> List.map CommonScoreEvent
            match hand |> List.ofSeq |> List.groupBy snd |> List.map fst with
            | [ suit ] when suit <> snd cut -> FourFlush hand
            | _ -> ()
        ]
    member this.Score = match this with | CommonScoreEvent event -> event.Score | FourFlush _ -> 4<point>
    member this.Text = match this with | CommonScoreEvent event -> event.Text | FourFlush cards -> sprintf "Flush (%s) for %i" (cardsText cards) (int this.Score)

type NibsScoreEvent =
    | Nibs of Card
    with
    static member Process (cut:Card) : NibsScoreEvent option = if fst cut = Jack then Some (Nibs cut) else None
    member this.Score = match this with | Nibs _ -> 2<point>
    member this.Text = match this with | Nibs card -> sprintf "%i for his nibs (%s)" this.Score (cardText card)
