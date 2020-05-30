module Aornota.Cribbage.Domain.Core

type Rank = | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two

type Suit = | Spade | Heart | Diamond | Club

type Card = | Card of Rank * Suit

type Hand = Card list

type [<Measure>] point
