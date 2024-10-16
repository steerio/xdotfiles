{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LambdaCase #-}

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
           def { layoutHook = avoidStruts $ tall ||| overlap ||| noBorders Full
               , terminal = "urxvt"
               , manageHook = insertPosition Below Newer
               , focusedBorderColor = "#bdae93"
               , logHook = logHook def >> dynamicLogWithPP pp { ppOutput = hPutStrLn bar }
               , modMask = mod
               , normalBorderColor = "#505050" } `additionalKeys` keys
    where
      tall = let b = Border 4 4 4 4 in
        smartBorders $ spacingRaw True b True b True $
          ResizableTall 1 (4/120) (17/25) []
      overlap =
        spacingRaw False (Border 4 4 8 8) True (Border 4 4 0 0) True $
          Overlap 1 (Take 252) (Take 400) 8

      pp = def { ppCurrent = xmobarColor "white" ""
               , ppTitle = xmobarColor "#b8bb26" "" . shorten 120
               , ppSep = " <fc=#7c6f64>|</fc> " }
      keys =
        [ ((mod, xK_p), spawn "dmenu_exec")
        , ((mod, xK_m), spawn "tv")
        , ((mod, xK_equal), spawn "pavucontrol")
        , ((mod, xK_t), spawn "twitter")
        , ((mod, xK_0), spawn "systemctl suspend")
        , ((mod, xK_s), sendMessage ToggleStruts)
        , ((mod, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ((mod, xK_bracketleft), withFocused $ windows . sink)
        , ((mod, xK_bracketright), spawn "xdotool mousemove_relative 6 0; import ~/screenshot-`date '+%Y-%m-%d-%H%M%S'`.png")
        , ((mod, xK_b), spawn "chromium")
        , ((mod, xK_Return), windows smartSwap)
        , ((mod, xK_Up),    sendMessage $ JumpToLayout "Full")
        , ((mod, xK_Down),  sendMessage $ JumpToLayout "Spacing ResizableTall")
        , ((mod, xK_Left),  sendMessage $ JumpToLayout "Spacing Overlap")
        , ((msh, xK_Up),    sendMessage MirrorExpand)
        , ((msh, xK_Down),  sendMessage MirrorShrink)
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10") ]
        ++
        [ ((msh, k), windows (greedyView i . shift i))
          | (i, k) <- zip (workspaces def) [xK_1..xK_9]]
      mod = mod4Mask
      msh = mod .|. shiftMask

-- Smart Swap

smartSwap :: StackSet i l a s sd -> StackSet i l a s sd
smartSwap = modify' $ \case
  Stack t [] (r:rs) -> Stack r [] (t:rs)
  Stack t ls rs -> Stack t [] (xs ++ x : rs) where (x:xs) = reverse ls

-- Overlap layout (inspired by Slack)

data Margin = Take !Dimension | Keep !Dimension | Ratio !Rational | Bigger !Margin !Margin | Edge
  deriving (Show, Read)

withMargin :: Dimension -> Margin -> Dimension
withMargin dim = \case
  (Keep n)     -> n
  (Take n)     -> dim - n
  (Ratio r)    -> floor (r * fromIntegral dim)
  (Bigger a b) -> max (withMargin dim a) (withMargin dim b)
  Edge         -> dim

data Overlap a = Overlap { overlapNMaster :: !Int
                         , overlapLeft :: !Margin
                         , overlapRight :: !Margin
                         , overlapGap :: !Dimension }
  deriving (Show, Read)

instance LayoutClass Overlap a where
  pureLayout (Overlap nmaster left right gap) r s = shuffle $ zip ws rs
    where
      ws = integrate s
      rs = overlap left right gap r nmaster (length ws)
      shuffle xs = if length (up s) < nmaster then xs else drop nmaster xs ++ take nmaster xs

  pureMessage (Overlap n l r g) msg = inc <$> fromMessage msg
    where
      inc (IncMasterN d) = Overlap (max 0 n+d) l r g

  description _ = "Overlap"

overlap
    :: Margin
    -> Margin
    -> Dimension
    -> Rectangle
    -> Int
    -> Int
    -> [Rectangle]
overlap left right gap r@(Rectangle x y w h) nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically n r
    else splitVertically nmaster r1 ++ splitVertically (n-nmaster) r2
  where
    r1 = let w' = calc left in
           Rectangle (fromIntegral (gap + w - w')) (y + fromIntegral gap) w' h'
    r2 = Rectangle x y (calc right) h'
    calc = let w' = w - gap
           in max 1 . min w' . withMargin w'
    h' = h - gap
