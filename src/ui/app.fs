module Aornota.Cribbage.Ui.App

open Browser.Dom

open Fable.React.Adaptive
module ReactHB = Fable.React.HookBindings

open Feliz
open Feliz.MaterialUI

open FSharp.Data.Adaptive

open System

let [<Literal>] private CRIBBAGE = "cribbage"

// *α* | β | γ | δ | ε | ζ | η | θ | ι | κ | λ | μ | ν | ξ | ο | π | ρ | σ | τ | υ | φ | χ | ψ | ω
let [<Literal>] private CRIBBAGE_VERSION = "α" // note: keep synchronized with  ./index.html | ../../package.json | ../../README.md

let [<Literal>] private CRIBBAGE_LOGO = "tpoc-32x32.png"

let private preamble =
    Html.div [
        Mui.typography [
            typography.variant.h4
            typography.paragraph true
            typography.children [
                Html.img [
                    prop.style [ style.verticalAlign.middle ]
                    prop.src CRIBBAGE_LOGO
                    prop.alt CRIBBAGE ]
                Html.text " | "
                Html.strong CRIBBAGE
                Html.text (sprintf " (%s)" CRIBBAGE_VERSION) ] ] ]

let private app =
    React.functionComponent (fun () ->
        Html.div [
            preamble ])

ReactDOM.render (app (), document.getElementById "app") // needs to match id of div in index.html
