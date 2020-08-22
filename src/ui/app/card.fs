[<RequireQualifiedAccess>]
module Aornota.Cribbage.Ui.Card

open Aornota.Cribbage.Domain.Core

open Feliz
open Feliz.MaterialUI
open Fable.MaterialUI.MaterialDesignIcons

// TODO-NMB: back | specific | disabled | seleccted | empty | ...

let render (rank:Rank, suit) =
    let suitIcon = match suit with | Spade -> cardsSpadeIcon | Heart -> cardsHeartIcon | Diamond -> cardsDiamondIcon | Club -> cardsClubIcon
    let colour = match suit with | Spade -> "#222" | Heart -> "#b00" | Diamond -> "#00b" | Club -> "#0b0"
    Mui.button [
        button.variant.outlined
        button.size.large
        button.disabled true
        button.startIcon (Mui.icon [ icon.children [ suitIcon [] ] ])
        prop.style [ style.backgroundColor colour ; style.color "#fff" ; style.fontSize (length.em 1.5) ; style.margin (length.em 0.25) ]
        button.children [ Html.text (sprintf "%c" rank.Text) ] ]
