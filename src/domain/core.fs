module Aornota.Cribbage.Domain.Core

open System
#if FABLE
#else
open System.Security.Cryptography
#endif

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

type Cards = Set<Card> // use Set to prevent duplicates
type Hand = Cards
type Crib = Cards

type Deck = Card list // use list since ordering matters
type Pegging = Card list

let private unshuffledDeck : Cards =
    [ Spade ; Heart ; Diamond ; Club ]
    |> List.collect (fun suit -> [ King ; Queen ; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two ; Ace ] |> List.map (fun rank -> rank, suit))
    |> Set.ofList

let private randoms count =
#if FABLE
    let rnd = Random()
    [ for _ in 1..count do yield rnd.Next() ]
#else
    use rng = new RNGCryptoServiceProvider()
    let bytes = Array.create 4 0uy
    [
        for _ in 1..count do
            rng.GetBytes(bytes)
            BitConverter.ToInt32(bytes, 0)
    ]
#endif

let [<Literal>] MAX_PEGGING = 31<pip>

let normalizedRandom () = abs (float (randoms 1 |> List.head) / float Int32.MaxValue)

let pips (cards:Cards) = cards |> List.ofSeq |> List.sumBy (fun (rank, _) -> rank.PipValue)

let cardText (rank:Rank, suit:Suit) = sprintf "%c%c" rank.Text suit.Text

let deckText (deck:Deck) = deck |> List.map cardText |> String.concat " "

let cardsText (cards:Cards) = cards |> List.ofSeq |> List.sort|> List.map cardText |> String.concat " "

let shuffledDeck () : Deck = unshuffledDeck |> List.ofSeq |> List.zip (randoms unshuffledDeck.Count) |> List.sortBy fst |> List.map snd

let dealToHand count (deck:Deck, hand:Hand) : Deck * Hand =
    if deck.Length < count then failwithf "Deck (%s) contains fewer than %i Card/s" (deckText deck) count
    let dealt = deck |> List.take count |> Set.ofList
    let alreadyInHand = dealt |> Set.intersect hand
    if alreadyInHand.Count > 0 then failwithf "One or more dealt Cards (%s) are already in Hand (%s)" (cardsText alreadyInHand) (cardsText hand)
    deck |> List.skip count, dealt |> Set.union hand

let removeFromHand (hand:Hand, cards:Cards) : Hand =
    let notInHand = hand |> Set.difference cards
    if notInHand.Count > 0 then failwithf "One or more Cards (%s) are not in Hand (%s)" (cardsText notInHand) (cardsText hand)
    cards |> Set.difference hand

let addToCrib (crib:Crib, cards:Cards) : Crib =
    if crib.Count <> 0 && crib.Count <> 2 then failwithf "Can only add to Crib (%s) when it contains 0 or 2 Cards (not %i)" (cardsText crib) crib.Count
    else if cards.Count <> 2 then failwithf "Can only add 2 Cards to Crib (not %i: %s)" cards.Count (cardsText cards)
    let alreadyInCrib = cards |> Set.intersect crib
    if alreadyInCrib.Count > 0 then failwithf "One or more Cards (%s) are already in Crib (%s)" (cardsText alreadyInCrib) (cardsText crib)
    cards |> Set.union crib

let randomChoice count (cards:Cards) : Cards =
    if cards.Count < count then failwithf "Cards (%s) contains fewer than %i Card/s" (cardsText cards) count
    else if cards.Count = count then cards
    else cards |> List.ofSeq |> List.zip (randoms cards.Count) |> List.sortBy fst |> List.map snd |> List.take count |> Set.ofList

let cut (deck:Deck) : Card = randomChoice 1 (deck |> Set.ofList) |> List.ofSeq |> List.head
