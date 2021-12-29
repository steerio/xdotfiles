import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

main = do
  bar <- spawnPipe "xmobar ~/.xmobar/xmobar.hs"
  xmonad $ ewmh . ewmhFullscreen . docks $
           def { focusedBorderColor = "#bdae93"
               , layoutHook =  avoidStruts $ smartBorders $ spacingRaw True border True border True tall ||| Full
               , logHook = logHook def >> dynamicLogWithPP pp { ppOutput = hPutStrLn bar }
               , modMask = mod
               , normalBorderColor = "#505050"
               , terminal = "urxvt" } `additionalKeys` keys
    where
      tall = Tall 1 (3/100) (17/25)
      border = Border 4 4 4 4
      pp = def { ppCurrent = xmobarColor "white" ""
               , ppTitle = xmobarColor "#b8bb26" "" . shorten 120
               , ppSep = " <fc=#7c6f64>|</fc> " }
      keys =
        [ ((mod, xK_p), spawn "dmenu_exec")
        , ((mod, xK_0), spawn "systemctl suspend")
        , ((mod, xK_c), spawn "tv cnn")
        , ((mod, xK_s), sendMessage ToggleStruts)
        , ((mod, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ((mod, xK_bracketright), spawn "import ~/screenshot-`date '+%Y-%m-%d-%H%M%S'`.png")
        , ((mod, xK_b), spawn "chromium")
        , ((mod, xK_Left),  withFocused $ keysMoveWindow (-1, 0))
        , ((mod, xK_Right), withFocused $ keysMoveWindow (1, 0))
        , ((mod, xK_Up),    withFocused $ keysMoveWindow (0, -1))
        , ((mod, xK_Down),  withFocused $ keysMoveWindow (0, 1))
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
        ]
        ++
        [ ((mod .|. shiftMask, k), windows (W.greedyView i . W.shift i))
          | (i, k) <- zip (workspaces def) [xK_1..xK_9]]
      mod = mod4Mask
