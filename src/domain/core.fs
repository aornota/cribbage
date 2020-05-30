module Aornota.Cribbage.Domain.Core

open System
open System.Security.Cryptography

type [<Measure>] pip // unit-of-measure for use in pegging
type [<Measure>] point // unit-of-measure for use in scoring

type Rank = | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | Ace // Ace is always "low" in cribbage
    with
    member this.Text =
        match this with | King -> 'K' | Queen -> 'Q' | Jack -> 'J' | Ten -> 'T' | Nine -> '9' | Eight -> '8' | Seven -> '7' | Six -> '6' | Five -> '5' | Four -> '4' | Three -> '3' | Two -> '2' | Ace -> 'A'
    member this.Value = (match this with | King | Queen | Jack | Ten -> 10 | Nine -> 9 | Eight -> 8 | Seven -> 7 | Six -> 6 | Five -> 5 | Four -> 4 | Three -> 3 | Two -> 2 | Ace -> 1) * 1<pip>

type Suit = | Spade | Heart | Diamond | Club
    with
    member this.Text = match this with | Spade -> 's' | Heart -> 'h' | Diamond -> 'd' | Club -> 'c'

type Card = | Card of Rank * Suit
    with
    member this.Text =
        let (Card (rank, suit)) = this
        sprintf "%c%c" rank.Text suit.Text

type Hand = Set<Card>

type Deck = Card list

let private unshuffledDeck =
    [ Spade ; Heart ; Diamond ; Club ]
    |> List.collect (fun suit -> [ King ; Queen ; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two ; Ace ] |> List.map (fun rank -> Card (rank, suit)))
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

let shuffledDeck () =
    unshuffledDeck
    |> List.ofSeq
    |> List.zip (randoms unshuffledDeck.Count)
    |> List.sortBy fst
    |> List.map snd

