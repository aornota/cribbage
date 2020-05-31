module Aornota.Cribbage.Tests.Tests

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring
open Aornota.Cribbage.Tests.Cards

open Expecto

let [<Tests>] domainCoreTests =
    testList "Domain.Core tests" [
        test "pips should return correct value" {
            Expect.equal (pips [ sK ; dJ ; d8 ; hA ]) 29<pip> "pips did not return correct value" }
        test "shuffledDeck () should contain 52 Cards" {
            Expect.hasLength (shuffledDeck ()) 52 "shuffledDeck () did not contain 52 Cards" }
        test "dealToHand should throw InsufficientCardsException" {
            Expect.throwsT<InsufficientCardsException> (fun _ -> dealToHand 3 ([ d7 ; c6 ], Set.empty) |> ignore) "dealToHand did not throw InsufficientCardsException" }
        test "dealToHand should throw CardsAlreadyInHandException" {
            Expect.throwsT<CardsAlreadyInHandException> (fun _ -> dealToHand 1 ([ d7 ; c6 ], d7 |> Set.singleton) |> ignore) "dealToHand did not throw CardsAlreadyInHandException" }
        test "dealToHand should return correct Deck and Hand" {
            Expect.equal (dealToHand 2 ([ sK ; h4 ; d3 ], Set.empty)) ([ d3 ], [ sK ; h4 ] |> Set.ofList) "dealToHand did not return correct Deck and Hand" }
        test "removeFromHand should throw CardsNotInHandException" {
            Expect.throwsT<CardsNotInHandException> (fun _ -> removeFromHand ([ sA ; hA ; dA ] |> Set.ofList, [ hA ; cA ] |> Set.ofList) |> ignore) "removeFromHand did not throw CardsNotInHandException" }
        test "removeFromHand should return correct Hand" {
            Expect.equal (removeFromHand ([ sA ; hA ; dA ] |> Set.ofList, [ hA ; dA ] |> Set.ofList)) (sA |> Set.singleton) "removeFromHand did not return correct Hand" }
        test "addToCrib should throw CannotAddToCribException" {
            Expect.throwsT<CannotAddToCribException> (fun _ -> addToCrib (s9 |> Set.singleton, [ s6 ; d4 ] |> Set.ofList) |> ignore) "addToCrib did not throw CannotAddToCribException" }
        test "addToCrib should throw CanOnlyAdd2CardsToCribException" {
            Expect.throwsT<CanOnlyAdd2CardsToCribException> (fun _ -> addToCrib ([ cJ ; s9 ] |> Set.ofList, [ s6 ] |> Set.ofList) |> ignore) "addToCrib did not throw CanOnlyAdd2CardsToCribException" }
        test "addToCrib should throw CardsAlreadyInCribException" {
            Expect.throwsT<CardsAlreadyInCribException> (fun _ -> addToCrib ([ cJ ; s9 ] |> Set.ofList, [ s9 ; s6 ] |> Set.ofList) |> ignore) "addToCrib did not throw CardsAlreadyInCribException" }
        test "addToCrib should return correct Crib" {
            Expect.equal (addToCrib ([ hJ ; c3 ] |> Set.ofList, [ hQ ; d7 ] |> Set.ofList)) ([ hQ ; hJ ; d7 ; c3 ] |> Set.ofList) "addToCrib did not return correct Crib" }
        test "randomChoice should throw InsufficientCardsException" {
            Expect.throwsT<InsufficientCardsException> (fun _ -> randomChoice 2 (cT |> Set.singleton) |> ignore) "randomChoice did not throw InsufficientCardsException" }
        test "randomChoice should return correct number of Cards" {
            Expect.hasLength (randomChoice 2 ([ hJ ; s9 ; s6 ; c3 ] |> Set.ofList)) 2 "randomChoice did not return correct number of Cards" }
        test "cut should return a single Card" {
            Expect.hasLength [ cut [ hJ ; s9 ; s6 ; c3 ] ] 1 "cut did not return a single Card" } ]

let [<Tests>] domainScoringTests =
    testList "Domain.Scoring tests" [
        test "PeggingScoreEvent.Play should throw CannotPlayNoneException" {
            Expect.throwsT<CannotPlayNoneException> (fun _ -> PeggingScoreEvent.Play([], None) |> ignore) "PeggingScoreEvent.Play did not throw CannotPlayNoneException" }
        test "PeggingScoreEvent.Play should throw CardAlreadyPlayedException" {
            Expect.throwsT<CardAlreadyPlayedException> (fun _ -> PeggingScoreEvent.Play([ s9 ; cT ], Some cT) |> ignore) "PeggingScoreEvent.Play did not throw CardAlreadyPlayedException" }
        test "PeggingScoreEvent.Play should throw CannotPlayCardException" {
            Expect.throwsT<CannotPlayCardException> (fun _ -> PeggingScoreEvent.Play([ cJ ; s9 ; cT ], Some d3) |> ignore) "PeggingScoreEvent.Play did not throw CannotPlayCardException" }
        test "PeggingScoreEvent.Play should return Go" {
            Expect.equal (PeggingScoreEvent.Play([ cJ ; s9 ; cT ], None)) [ Go ] "PeggingScoreEvent.Play did not return Go" }
        test "PeggingScoreEvent.Play should return empty" {
            Expect.isEmpty (PeggingScoreEvent.Play([ s9 ; cT ], Some d4)) "PeggingScoreEvent.Play did not return empty" }
        // pairs | triples | quadruples | runs | fifteen | thirty-one | multiple...


        ]

// TODO-NMB: normalizedRandom tests?...

// TODO-NMB: "Performance" test (inc. check impossible hand/crib scores)...

