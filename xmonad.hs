import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.TopicSpace
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Accordion
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import System.IO

myTopics :: [Topic]
myTopics =
    [ "web"
    , "code"
    , "remotes"
    , "cli"
    , "cli 2"
    , "cli 3"
    , "cssh"
    , "pidgin"
    , "deployment"
    ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList $
        [ ("web", "w/code")
        ]
    , defaultTopic = "work"
    , defaultTopicAction = const $ spawn "google-chrome"
    , topicActions = M.fromList $
        [ ("web", spawn "google-chrome")
        , ("code", spawn "subl")
        , ("remotes", spawn "konsole --profile ec2")
        , ("cli", spawn "konsole --workdir '~/'" >*> 4)
        , ("pidgin", spawn "pidgin")
        , ("deployment", spawn "konsole --profile important")
        ]
    }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "konsole --workdir " ++ dir

layout = onWorkspace "web" (noBorders Full ||| tiled ||| Mirror tiled ||| OneBig (3/4) (3/4)) $
         onWorkspace "code" (noBorders Full ||| tiled ||| Mirror tiled ||| Grid ||| Accordion) $
         onWorkspace "cli" primaryCli $
         onWorkspace "cssh" (Grid ||| tiled ||| Mirror tiled ||| noBorders Full) $
         onWorkspace "pidgin" (withIM (1%7) (Role "buddy_list") Grid ||| Full ||| tiled ||| Mirror tiled) $
         def
    where
        def = tiled ||| Mirror tiled ||| noBorders Full ||| Grid ||| Accordion
        primaryCli = Mirror cliLayout ||| tiled ||| noBorders Full ||| Grid ||| Accordion
        tiled = Tall nmaster delta ratio
        cliLayout = Tall cliNMaster delta cliRatio
        nmaster = 1
        ratio = 1/2
        cliNMaster = 2
        cliRatio = 2/3
        delta = 3/100

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
        [ ((modm .|. shiftMask, xK_g), windowPromptGoto defaultXPConfig { autoComplete = Just 500000 } )
        , ((modm .|. shiftMask, xK_b), windowPromptBring defaultXPConfig { autoComplete = Just 500000 } )
        , ((modm .|. controlMask, xK_x), shellPrompt defaultXPConfig)
        , ((modm .|. shiftMask, xK_u), sendMessage Shrink) -- since apple still masks cmd-h
        , ((modm .|. shiftMask, xK_o), spawn "konsole --profile ec2")
        , ((modm .|. shiftMask, xK_i), spawn "konsole --profile important")
        , ((modm, xK_a), currentTopicAction myTopicConfig)
        , ((modm, xK_g), promptedGoto)
        , ((modm .|. shiftMask, xK_g), promptedGoto)
        ]

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

spawnToWorkspace :: String -> String -> X()
spawnToWorkspace program workspace = spawn program >> (windows $ W.greedyView workspace)

newKeys x = myKeys x `M.union` keys defaultConfig x

bar = "xmobar"

--unused
barPP = xmobarPP { ppCurrent = xmobarColor "green" "" .shorten 100 }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main :: IO ()
main = do
    checkTopicConfig myTopics myTopicConfig
    xmproc <- spawnPipe "/home/matt/.cabal/bin/xmobar /home/matt/.xmobarrc"
    xmonad $ defaultConfig
        { terminal = "konsole"
        , workspaces = myTopics
        , borderWidth = 2
        , focusedBorderColor = "#cd8b00"
        , layoutHook = avoidStruts $ layout
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "orange" "" .shorten 100
            , ppCurrent = xmobarColor "orange" "" .wrap "[" "]"
            }
        , keys = newKeys
        , modMask = mod4Mask
        , startupHook = return ()
        }

