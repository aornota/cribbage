[<RequireQualifiedAccess>]
module Aornota.Cribbage.Ui.Theme

// Based on https://github.com/Shmew/Feliz.MaterialUI.MaterialTable/blob/master/docs/App.fs.

open Feliz
open Feliz.MaterialUI

type private AppTheme = {
    PaletteType : PaletteType
    Primary : string
    Secondary : string
    PrimaryContrastText : string
    Error : string }
    with
    static member Dark = {
        PaletteType = PaletteType.Dark
        Primary = "#105010"
        PrimaryContrastText = "#fff"
        Secondary = "#101050"
        Error = "#501010" }
    static member Light = {
        PaletteType = PaletteType.Light
        Primary = "#afefef"
        PrimaryContrastText = "#000"
        Secondary = "#efefaf"
        Error = "#efafef" }

let private defaultTheme = Styles.createMuiTheme ()

let private buildTheme (appTheme:AppTheme) = Styles.createMuiTheme ([
    let isDarkTheme = appTheme.PaletteType = PaletteType.Dark

    if isDarkTheme then theme.palette.type'.dark else theme.palette.type'.light

    theme.palette.primary'.main appTheme.Primary
    theme.palette.primary'.contrastText appTheme.PrimaryContrastText
    theme.palette.secondary'.main appTheme.Secondary
    theme.palette.error'.main appTheme.Error

    theme.overrides.muiPaper.elevation2 [
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
        else style.backgroundColor (defaultTheme.palette.background.``default`` + "!important") ] ])

let getTheme isDark = buildTheme (if isDark then AppTheme.Dark else AppTheme.Light)

let useStyles : unit -> _ = Styles.makeStyles (fun styles (theme:Theme) ->
    (* let drawerWidth = 200 *)
    {|
        appBar = styles.create [
            style.zIndex (theme.zIndex.drawer + 1)
            style.cursor "default"
            style.display.grid ]
        (* containerCard = styles.create [ style.padding (length.em 1) ]
        contentCard = styles.create [ style.height (length.percent 100) ]
        content = styles.create [
            style.flexGrow 1
            style.height.inheritFromParent
            style.paddingTop (length.em 6)
            style.paddingLeft (length.em 2)
            style.paddingRight (length.em 2)
            style.paddingBottom (length.em 1) ]
        drawer = styles.create [
            style.zIndex theme.zIndex.drawer
            style.width drawerWidth
            style.flexShrink 0 ]
        drawerPaper = styles.create [ style.width drawerWidth ]
        fullSizeCard = styles.create [
            style.paddingTop (length.em 1)
            style.paddingBottom (length.em 1)
            style.paddingLeft (length.em 5)
            style.paddingRight (length.em 5)
            style.flexGrow 1 ] *)
        root = styles.create [
            style.display.flex
            style.height.inheritFromParent
            style.userSelect.none ]
        (* sampleApp = styles.create [
            style.paddingTop (length.em 2)
            style.paddingBottom (length.em 2) ]
        mtBackground = styles.create [ style.backgroundColor theme.palette.background.``default`` ] *)
        title = styles.create [ style.width (length.percent 100) ]
        toolbar = styles.create [ yield! theme.mixins.toolbar ]
        (* unselectable = styles.create [ style.userSelect.none ] *)
    |})
