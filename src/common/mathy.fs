module Aornota.Cribbage.Common.Mathy

open System
#if FABLE
#else
open System.Security.Cryptography
#endif

let randoms count =
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

let normalizedRandom () = abs (float (randoms 1 |> List.head) / float Int32.MaxValue)

// See Tomas Petricek's answer to https://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f.
let rec combinations acc size list = seq {
    match size, list with
    | n, h :: t ->
        if n > 0 then yield! combinations (h :: acc) (n - 1) t
        if n >= 0 then yield! combinations acc n t
    | 0, [] -> yield acc
    | _, [] -> () }
