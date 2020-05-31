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
let rec combinations acc size set = seq {
    match size, set with
    | n, x :: xs ->
        if n > 0 then yield! combinations (x :: acc) (n - 1) xs
        else if n >= 0 then yield! combinations acc n xs
    | 0, [] -> yield acc
    | _, [] -> () }