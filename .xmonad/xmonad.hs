{-# LANGUAGE LambdaCase #-}

import XMonad
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.StackSet hiding (workspaces)
import Graphics.X11.ExtraTypes.XF86

main = do
  bar <- spawnPipe "xmobar ~/.xmobar/xmobar.hs"
  xmonad $ ewmh . ewmhFullscreen . docks $
           def { layoutHook = avoidStruts $ smartBorders $ gaps tall ||| Full
               , terminal = "urxvt"
               , focusFollowsMouse = False
               , manageHook = insertPosition Below Newer
               , focusedBorderColor = "#bdae93"
               , logHook = logHook def >> dynamicLogWithPP pp { ppOutput = hPutStrLn bar }
               , modMask = mod
               , normalBorderColor = "#505050" } `additionalKeys` keys
    where
      tall = ResizableTall 1 (4/120) (17/25) []
      border = Border 4 4 4 4
      gaps = spacingRaw True border True border True
      pp = def { ppCurrent = xmobarColor "white" ""
               , ppTitle = xmobarColor "#b8bb26" "" . shorten 120
               , ppSep = " <fc=#7c6f64>|</fc> " }
      keys =
        [ ((mod, xK_p), spawn "dmenu_exec")
        , ((mod, xK_m), spawn "tv")
        , ((mod, xK_t), spawn "twitter")
        , ((mod, xK_0), spawn "systemctl suspend")
        , ((mod, xK_s), sendMessage ToggleStruts)
        , ((mod, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ((mod, xK_bracketleft), withFocused $ windows . sink)
        , ((mod, xK_bracketright), spawn "xdotool mousemove_relative 6 0; import ~/screenshot-`date '+%Y-%m-%d-%H%M%S'`.png")
        , ((mod, xK_b), spawn "chromium")
        , ((mod, xK_Return), windows smartSwap)
        , ((mod, xK_Up),    sendMessage MirrorExpand)
        , ((mod, xK_Down),  sendMessage MirrorShrink)
        , ((msh, xK_Left),  withFocused $ keysMoveWindow (-1, 0))
        , ((msh, xK_Right), withFocused $ keysMoveWindow (1, 0))
        , ((msh, xK_Up),    withFocused $ keysMoveWindow (0, -1))
        , ((msh, xK_Down),  withFocused $ keysMoveWindow (0, 1))
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10") ]
        ++
        [ ((msh, k), windows (greedyView i . shift i))
          | (i, k) <- zip (workspaces def) [xK_1..xK_9]]
      mod = mod4Mask
      msh = mod .|. shiftMask

smartSwap :: StackSet i l a s sd -> StackSet i l a s sd
smartSwap = modify' $ \case
  Stack t [] (r:rs) -> Stack r [] (t:rs)
  Stack t ls rs -> Stack t [] (xs ++ x : rs) where (x:xs) = reverse ls
