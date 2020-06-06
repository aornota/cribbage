module Aornota.Cribbage.Domain.Core

open Aornota.Cribbage.Common.Mathy

type [<Measure>] game // unit-of-measure for use in meta-scoring
type [<Measure>] pip // unit-of-measure for use in pegging
type [<Measure>] point // unit-of-measure for use in scoring

type Rank = | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | Ace // Ace is always "low" in cribbage
    with
    member this.FullText =
        match this with
        | King -> "King" | Queen -> "Queen" | Jack -> "Jack" | Ten -> "Ten" | Nine -> "Nine" | Eight -> "Eight" | Seven -> "Seven" | Six -> "Six" | Five -> "Five" | Four -> "Four" | Three -> "Three"
        | Two -> "Two" | Ace -> "Ace"
    member this.Text =
        match this with | King -> 'K' | Queen -> 'Q' | Jack -> 'J' | Ten -> 'T' | Nine -> '9' | Eight -> '8' | Seven -> '7' | Six -> '6' | Five -> '5' | Four -> '4' | Three -> '3' | Two -> '2' | Ace -> 'A'
    member this.PipValue = (match this with | King | Queen | Jack | Ten -> 10 | Nine -> 9 | Eight -> 8 | Seven -> 7 | Six -> 6 | Five -> 5 | Four -> 4 | Three -> 3 | Two -> 2 | Ace -> 1) * 1<pip>
    member this.Value = match this with | King -> 13  | Queen -> 12 | Jack -> 11 | _ -> int this.PipValue

type Suit = | Spade | Heart | Diamond | Club
    with
    member this.FullText = match this with | Spade -> "Spade" | Heart -> "Heart" | Diamond -> "Diamond" | Club -> "Club"
    member this.Text = match this with | Spade -> 's' | Heart -> 'h' | Diamond -> 'd' | Club -> 'c'
    member this.Symbol = match this with | Spade -> '♠' | Heart -> '♥' | Diamond -> '♦' | Club -> '♣'

type Card = Rank * Suit

type CardS = Set<Card> // use Set to prevent duplicates
type Hand = CardS
type Crib = CardS

type CardL = Card list // use list when ordering matters
type Deck = CardL
type Pegging = CardL

exception InsufficientCardsException of string
exception CardsAlreadyInHandException of string
exception CardsNotInHandException of string
exception CannotAddToCribException of string
exception CanOnlyAdd2CardsToCribException of string
exception CardsAlreadyInCribException of string

let private unshuffledDeck : CardS =
    [ Spade ; Heart ; Diamond ; Club ]
    |> List.collect (fun suit -> [ King ; Queen ; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two ; Ace ] |> List.map (fun rank -> rank, suit))
    |> Set.ofList

let [<Literal>] MAX_PEGGING = 31<pip>

let pips (cards:CardL) = cards |> List.sumBy (fun (rank, _) -> rank.PipValue)

let cardText (rank:Rank, suit:Suit) = sprintf "%c%c" rank.Text suit.Text

let deckText (deck:Deck) = deck |> List.map cardText |> String.concat " "

let cardsText (cards:CardS) = cards |> List.ofSeq |> List.sort|> List.map cardText |> String.concat " "

let shuffledDeck () : Deck = unshuffledDeck |> List.ofSeq |> List.zip (randoms unshuffledDeck.Count) |> List.sortBy fst |> List.map snd

let dealToHand count (deck:Deck, hand:Hand) : Deck * Hand =
    if deck.Length < count then raise (InsufficientCardsException (sprintf "Deck (%s) contains fewer than %i Card/s" (deckText deck) count))
    let dealt = deck |> List.take count |> Set.ofList
    let alreadyInHand = dealt |> Set.intersect hand
    if alreadyInHand.Count > 0 then raise (CardsAlreadyInHandException (sprintf "One or more dealt Cards (%s) are already in Hand (%s)" (cardsText alreadyInHand) (cardsText hand)))
    deck |> List.skip count, dealt |> Set.union hand

let removeFromHand (hand:Hand, cards:CardS) : Hand =
    let notInHand = hand |> Set.difference cards
    if notInHand.Count > 0 then raise (CardsNotInHandException (sprintf "One or more Cards (%s) are not in Hand (%s)" (cardsText notInHand) (cardsText hand)))
    cards |> Set.difference hand

let addToCrib (crib:Crib, cards:CardS) : Crib =
    if crib.Count <> 0 && crib.Count <> 2 then raise (CannotAddToCribException (sprintf "Can only add to Crib (%s) when it contains 0 or 2 Cards (not %i)" (cardsText crib) crib.Count))
    else if cards.Count <> 2 then raise (CanOnlyAdd2CardsToCribException (sprintf "Can only add 2 Cards to Crib (not %i: %s)" cards.Count (cardsText cards)))
    let alreadyInCrib = cards |> Set.intersect crib
    if alreadyInCrib.Count > 0 then raise (CardsAlreadyInCribException (sprintf "One or more Cards (%s) are already in Crib (%s)" (cardsText alreadyInCrib) (cardsText crib)))
    cards |> Set.union crib

let randomChoice count (cards:CardS) : CardS =
    if cards.Count < count then raise (InsufficientCardsException (sprintf "Cards (%s) contains fewer than %i Card/s" (cardsText cards) count))
    else if cards.Count = count then cards
    else cards |> List.ofSeq |> List.zip (randoms cards.Count) |> List.sortBy fst |> List.map snd |> List.take count |> Set.ofList

let randomSingle (cards:CardS) : Card = randomChoice 1 cards |> Set.maxElement

let deckExcept (cards:CardS) : CardS = cards |> Set.difference unshuffledDeck

let cut (deck:Deck) : Card * Deck =
    let cut = randomChoice 1 (deck |> Set.ofList) |> List.ofSeq |> List.head
    cut, deck |> List.filter (fun card -> card <> cut)
