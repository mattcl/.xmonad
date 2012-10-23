import XMonad
import XMonad.Layout.Grid
import XMonad.Layout.Accordion
import XMonad.Layout.Column
import XMonad.Layout.OneBig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

layout = tiled ||| Mirror tiled ||| Full ||| Grid ||| Accordion ||| Column 1.6 ||| OneBig (3/4) (3/4)
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

main = xmonad defaults

defaults = defaultConfig {
		terminal = "konsole",
		borderWidth = 2,
		focusedBorderColor = "#cd8b00",
        layoutHook = layout
    }
