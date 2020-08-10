[<RequireQualifiedAccessAttribute>]
module Aornota.Cribbage.Ui.Workers.ForCribWorkers

open Aornota.Cribbage.Domain.Strategy

open Feliz.UseWorker

let forCribRandomWorker = WorkerFunc.Create ("forCribWorkers", "forCribRandomWorker", fun (isDealer, hand) -> forCribRandom (isDealer, hand))
let forCribBasicWorker = WorkerFunc.Create ("forCribWorkers", "forCribBasicWorker", fun (isDealer, hand) -> forCribBasic (isDealer, hand))
let forCribIntermediateWorker = WorkerFunc.Create ("forCribWorkers", "forCribIntermediateWorker", fun (isDealer, hand) -> forCribIntermediate (isDealer, hand))
