module Aornota.Cribbage.Tests.Tests

open Aornota.Cribbage.Domain.Core

open Expecto

let [<Tests>] todo =
    testList "todo" [
        test "todo" {
            let invalid = []
            Expect.isEmpty invalid (sprintf "Meh: %A" invalid) } ]
