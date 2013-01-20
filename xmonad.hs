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
    [ "work"
    , "imvudev"
    , "dev2"
    , "standard"
    , "vm"
    , "chrome"
    , "scratchdev"
    , "webtest"
    , "music"
    , "gem"
    , "dashboard"
    ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList $
        [ ("scratchdev", "w/scratch")
        ]
    , defaultTopic = "work"
    , defaultTopicAction = const $ spawn "firefox" >> spawn "konsole" >*> 2
    , topicActions = M.fromList $
        [ ("work", spawn "firefox" >> spawn "konsole" >*> 2)
        , ("imvudev", spawn "konsole")
        , ("dev2", spawn "konsole")
        , ("vm", spawn "vmware")
        , ("chrome", spawn "google-chrome")
        , ("scratchdev", spawnShellIn "scratch")
        , ("webtest", spawn "google-chrome --remote 'http://localhost.imvu.com/runtests'")
        , ("music", spawn "google-chrome --remote 'http://music.google.com'")
        , ("gem", spawnShellIn "scratch/imvu-dev")
        , ("dashboard", spawnShellIn "scratch/imvu_dashboard")
        ]
    }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "konsole --workdir " ++ dir

layout = onWorkspace "work" (OneBig (3/4) (3/4) ||| def) $
         def
    where
        def = tiled ||| Mirror tiled ||| noBorders Full ||| Grid ||| Accordion
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
        [ ((modm .|. controlMask, xK_n), appendFilePrompt defaultXPConfig "/home/mchunlum/NOTES")
        , ((modm .|. shiftMask, xK_g), windowPromptGoto defaultXPConfig { autoComplete = Just 500000 } )
        , ((modm .|. shiftMask, xK_b), windowPromptBring defaultXPConfig { autoComplete = Just 500000 } )
        , ((modm .|. controlMask, xK_x), shellPrompt defaultXPConfig)
        , ((modm, xK_q), spawn "/home/mchunlum/bin/xmonad --recompile; /home/mchunlum/bin/xmonad --restart")
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

myConfig = do
    checkTopicConfig myTopics myTopicConfig
    return $ defaultConfig
        { terminal = "konsole"
        , workspaces = myTopics
		, borderWidth = 2
		, focusedBorderColor = "#cd8b00"
        , layoutHook = layout
        , keys = newKeys
        , startupHook = return ()
        }

main :: IO ()
main = xmonad =<< myConfig
