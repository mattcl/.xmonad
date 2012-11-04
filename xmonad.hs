import qualified Data.Map as M
import XMonad
import XMonad.Layout.Grid
import XMonad.Layout.Accordion
import XMonad.Layout.Column
import XMonad.Layout.OneBig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

layout = tiled ||| Mirror tiled ||| Full ||| Grid ||| Accordion ||| OneBig (3/4) (3/4)
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
        [ ((modm .|. controlMask, xK_n), appendFilePrompt defaultXPConfig "/home/matt/NOTES")
        , ((modm .|. shiftMask, xK_g), windowPromptGoto defaultXPConfig { autoComplete = Just 500000 } )
        , ((modm .|. shiftMask, xK_b), windowPromptBring defaultXPConfig { autoComplete = Just 500000 } )
        , ((modm .|. controlMask, xK_x), shellPrompt defaultXPConfig)
        ]

newKeys x = myKeys x `M.union` keys defaultConfig x

main = xmonad defaults

defaults = defaultConfig {
		terminal = "konsole",
		borderWidth = 2,
		focusedBorderColor = "#cd8b00",
        layoutHook = layout,
        keys = newKeys
    }
