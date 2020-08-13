[<RequireQualifiedAccess>]
module Aornota.Cribbage.Ui.Theme

// Adapted from https://github.com/Shmew/Feliz.MaterialUI.MaterialTable/blob/master/docs/App.fs.

open Feliz
open Feliz.MaterialUI

type private AppTheme = {
    PaletteType : PaletteType
    Primary : string
    PrimaryContrastText : string
    Secondary : string
    SecondaryContrastText : string
    Error : string }
    with
    static member Dark = {
        PaletteType = PaletteType.Dark
        Primary = "#64ffda"
        PrimaryContrastText = "#000"
        Secondary = "#ffd600"
        SecondaryContrastText = "#000"
        Error = "#f00" }
    static member Light = {
        PaletteType = PaletteType.Light
        Primary = "#2e7d32"
        PrimaryContrastText = "#fff"
        Secondary = "#ff8f00"
        SecondaryContrastText = "#000"
        Error = "#f00" }

let private defaultTheme = Styles.createMuiTheme ()

let private buildTheme (appTheme:AppTheme) = Styles.createMuiTheme ([
    let isDarkTheme = appTheme.PaletteType = PaletteType.Dark

    if isDarkTheme then theme.palette.type'.dark else theme.palette.type'.light

    theme.palette.primary'.main appTheme.Primary
    theme.palette.primary'.contrastText appTheme.PrimaryContrastText
    theme.palette.secondary'.main appTheme.Secondary
    theme.palette.secondary'.contrastText appTheme.SecondaryContrastText
    theme.palette.error'.main appTheme.Error

    // TODO-NMB: Overrides?...

    (* theme.overrides.muiPaper.elevation2 [
        style.custom ("box-shadow", defaultTheme.shadows.[8])
        if isDarkTheme then style.backgroundColor "#303030"
        else style.backgroundColor defaultTheme.palette.background.``default`` ]
    theme.overrides.muiTableCell.footer [
        style.borderRadius 4
        style.borderWidth 0 ]
    theme.overrides.muiTableCell.head [
        if isDarkTheme then style.backgroundColor "#303030 !important"
        else style.backgroundColor (defaultTheme.palette.background.``default`` + "!important") ]
    theme.overrides.muiTableCell.root [
        if isDarkTheme then style.backgroundColor "#303030 !important"
        else style.backgroundColor (defaultTheme.palette.background.``default`` + "!important") ]
    theme.overrides.muiTable.root [
        if isDarkTheme then style.backgroundColor "#303030 !important"
        else style.backgroundColor (defaultTheme.palette.background.``default`` + "!important") ] *)
    ])

let getTheme isDark = buildTheme (if isDark then AppTheme.Dark else AppTheme.Light)
