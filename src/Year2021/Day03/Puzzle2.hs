module Year2021.Day03.Puzzle2 where

import Data.List
  ( transpose )

import Year2021.Day03.Puzzle1
  ( Bit , Binary , to_decimal )

solve :: Bit -> [Binary] -> Binary
solve bit bs = solve' 0 bs
  where solve' _ [b] = b
        solve' n bs  =
          let ones  = filter (id  . (!!n)) bs
              zeros = filter (not . (!!n)) bs
          in solve' (n + 1) $
             case length zeros `compare` length ones of
               LT -> if bit then ones  else zeros
               GT -> if bit then zeros else ones
               EQ -> filter ((==bit) . (!!n)) bs

solution :: String -> IO ()
solution input =
  let diagnostic   = map (=='1') <$> (lines input)
  in print $ to_decimal (solve True diagnostic) * to_decimal (solve False diagnostic)
