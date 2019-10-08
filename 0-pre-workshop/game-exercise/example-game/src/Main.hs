module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = play (InWindow "yin-yang" (300, 300) (100, 100))
            black
            20
            initial
            view
            input
            step

data World = Go Int | Pause Int

initial :: World
initial = Go 0

input :: Event -> World -> World
-- Pause logic
input (EventKey (Char 'p') _ _ _) (Go n)    = Pause n
input (EventKey (Char 'r') _ _ _) (Pause n) = Go    n
-- Otherwise, keep the same
input _ w = w

step :: Float -> World -> World
step _ (Go    n) = Go ((n + 1) `mod` 255)
step _ (Pause n) = Pause n

view :: World -> Picture
view (Go n) = color c (thickCircle 50 50)
  where c = makeColorI n 150 150 255
view (Pause _) = translate (-120) 0 
               $ scale 0.2 0.2
               $ color white
               $ text "Press r to resume"
  
  