module Aornota.Cribbage.Tests.Tests

open Aornota.Cribbage.Common.Mathy
open Aornota.Cribbage.Domain.Core
open Aornota.Cribbage.Domain.Scoring
open Aornota.Cribbage.Tests.Cards

open Expecto

let [<Tests>] commonMathyTests =
    testList "Common.Mathy tests" [
        for n in 1..100 do // no need to go overboard!
            yield test (sprintf "normalizedRandom () should be between 0.0 and 1.0 (inclusive) -> iteration %i" n) {
                let value = normalizedRandom ()
                Expect.isLessThanOrEqual value 1.0 "normalizedRandom () should be less than or equal to 1.0"
                Expect.isGreaterThanOrEqual value 0.0 "normalizedRandom () should be greater than or equal to 0.0" } ]

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
        test "PeggingScoreEvent.Play should return PeggingPair" {
            Expect.equal (PeggingScoreEvent.Play([ h4 ; h2 ], Some d4)) [ PeggingPair Four ] "PeggingScoreEvent.Play did not return PeggingPair" }
        test "PeggingScoreEvent.Play should return ThreeOfAKind and PeggingFifteen" {
            Expect.equal (PeggingScoreEvent.Play([ h5 ; c5 ], Some s5)) [ ThreeOfAKind Five ; PeggingFifteen ] "PeggingScoreEvent.Play did not return ThreeOfAKind and PeggingFifteen" }
        test "PeggingScoreEvent.Play should return FourOfAKind and ThirtyOne" {
            Expect.equal (PeggingScoreEvent.Play([ h7 ; s7 ; c7 ; c3 ], Some d7)) [ FourOfAKind Seven ; ThirtyOne ] "PeggingScoreEvent.Play did not return FourOfAKind and ThirtyOne" }
        test "PeggingScoreEvent.Play should return PeggingRun" {
            Expect.equal (PeggingScoreEvent.Play([ sA ; s3 ; c5 ; d2 ; sJ ], Some h4)) [ PeggingRun (Five, Ace) ] "PeggingScoreEvent.Play did not return PeggingRun" }

        test "CommonScoreEvent.Process should throw DoesNotContain4CardsException" {
            Expect.throwsT<DoesNotContain4CardsException> (fun _ -> CommonScoreEvent.Process(false, [ dQ ; dJ ; c5 ] |> Set.ofList, h8) |> ignore) "CommonScoreEvent.Process did not throw DoesNotContain4CardsException" }
        test "CommonScoreEvent.Process should throw MustNotContainCutCardException" {
            Expect.throwsT<MustNotContainCutCardException> (fun _ -> CommonScoreEvent.Process(false, [ dQ ; dJ ; h8 ; c5 ] |> Set.ofList, h8) |> ignore) "CommonScoreEvent.Process did not throw MustNotContainCutCardException" }
        test "CommonScoreEvent.Process should return empty" {
            Expect.isEmpty (CommonScoreEvent.Process(false, [ sK ; dJ ; s9 ; c7 ] |> Set.ofList, h4)) "CommonScoreEvent.Process did not return empty" }
        test "CommonScoreEvent.Process should return Fifteen and Pairs" {
            let expected = [ Fifteen ([ sK ; h5 ] |> Set.ofList) ; Pair ([ s9 ; d9 ] |> Set.ofList) ; Pair ([ s9 ; c9 ] |> Set.ofList) ; Pair ([ d9 ; c9 ] |> Set.ofList) ]
            Expect.equal (CommonScoreEvent.Process(false, [ sK ; s9 ; d9 ; c9 ] |> Set.ofList, h5)) expected "CommonScoreEvent.Process did not return Fifteen and Pairs" }
        test "CommonScoreEvent.Process should return Pair and Runs" {
            let expected = [ Pair ([ hQ ; dQ ] |> Set.ofList) ; CommonScoreEvent.Run ([ sK ; dQ ; cJ ; cT ] |> Set.ofList) ; CommonScoreEvent.Run ([ sK ; hQ ; cJ ; cT ] |> Set.ofList) ]
            Expect.equal (CommonScoreEvent.Process(false, [ sK ; hQ ; cJ ; cT ] |> Set.ofList, dQ)) expected "CommonScoreEvent.Process did not return Pair and Runs" }
        test "CommonScoreEvent.Process should return FiveFlush" {
            let expected = [ FiveFlush ([ sK ; sQ ; sT ; s8 ; s2 ] |> Set.ofList) ]
            Expect.equal (CommonScoreEvent.Process(false, [ sK ; sQ ; sT ; s8 ] |> Set.ofList, s2)) expected "CommonScoreEvent.Process did not return FiveFlush" }
        test "CommonScoreEvent.Process should return Nob" {
            let expected = [ Nob sJ ]
            Expect.equal (CommonScoreEvent.Process(false, [ sK ; sJ ; sT ; d8 ] |> Set.ofList, s2)) expected "CommonScoreEvent.Process did not return Nob" }

        test "CribScoreEvent.Process should return Run but not FourFlush" {
            let expected = [ CommonScoreEvent.Run ([ sK ; sQ ; dJ ] |> Set.ofList) ] |> List.map CribScoreEvent
            Expect.equal (CribScoreEvent.Process([ sK ; sQ ; s9 ; s8 ] |> Set.ofList, dJ)) expected "CribScoreEvent.Process did not return Run but not FourFlush" }
        test "HandScoreEvent.Process should return Run and FourFlush" {
            let expected = [ CommonScoreEvent (CommonScoreEvent.Run ([ sK ; sQ ; dJ ] |> Set.ofList)) ; FourFlush ([ sK ; sQ ; s9 ; s8 ] |> Set.ofList) ]
            Expect.equal (HandScoreEvent.Process([ sK ; sQ ; s9 ; s8 ] |> Set.ofList, dJ)) expected "HandScoreEvent.Process did not return Run and FourFlush" }

        test "NibsScoreEvent.Process should return None" {
            Expect.isNone (NibsScoreEvent.Process sQ) "NibsScoreEvent.Process did not return None" }
        test "NibsScoreEvent.Process should return Nibs" {
            Expect.equal (NibsScoreEvent.Process sJ) (Some (Nibs sJ)) "NibsScoreEvent.Process did not return Nibs" } ]

let [<Tests>] domainScoringAdditionalTests =
    let handsEventsAndCribEvents () =
        let deck = shuffledDeck ()
        let deck, dealt1 = dealToHand 6 (deck, Set.empty)
        let choice1 = randomChoice 2 dealt1
        let hand1, crib = removeFromHand (dealt1, choice1), addToCrib (Set.empty, choice1)
        let deck, dealt2 = dealToHand 6 (deck, Set.empty)
        let choice2 = randomChoice 2 dealt2
        let hand2, crib = removeFromHand (dealt2, choice2), addToCrib (crib, choice2)
        let cut = cut deck
        let hand1Events = HandScoreEvent.Process(hand1, cut)
        let hand2Events = HandScoreEvent.Process(hand2, cut)
        let cribEvents = CribScoreEvent.Process(crib, cut)
        cut, hand1, hand1Events, hand2, hand2Events, crib, cribEvents
    let handScore (events:HandScoreEvent list) = events |> List.sumBy (fun event -> event.Score)
    let cribScore (events:CribScoreEvent list) = events |> List.sumBy (fun event -> event.Score)
    let mustNotBe19Or25or26or27 (cut:Card) (cards:CardS) isCrib score =
        let mustNotBe n = Expect.notEqual score n (sprintf "%s (%s | %s) can never score %i" (if isCrib then "Crib" else "Hand") (cardsText cards) (cardText cut) n)
        mustNotBe 19<point>
        mustNotBe 25<point>
        mustNotBe 26<point>
        mustNotBe 27<point>
    testList "Domain.Scoring additional tests" [
        for n in 1..100000 do // perhaps excessive!
            yield test (sprintf "Hands and Crib can never score 19, 25, 26 or 27 -> iteration %i" n) {
                let cut, hand1, hand1Events, hand2, hand2Events, crib, cribEvents = handsEventsAndCribEvents ()
                mustNotBe19Or25or26or27 cut hand1 false (handScore hand1Events)
                mustNotBe19Or25or26or27 cut hand2 false (handScore hand2Events)
                mustNotBe19Or25or26or27 cut crib true (cribScore cribEvents) } ]
