module Year2021.Day12.Puzzle1 where

import AdventLib.Parsing
import Data.Char ( isUpper )
import Data.List (nub, (\\))

type Name  = String
data Cave  = Start | End | Large Name | Small Name
  deriving Eq

type Edge  = (Cave, Cave)
type Graph = ([Cave], [Edge])
type Path  = [Cave]

cave :: Parser Cave
cave =
  do name <- lexeme (many1 letter)
     return $
       case (name, isUpper $ head name) of
         ("start", _    ) -> Start
         ("end"  , _    ) -> End
         (_      , True ) -> Large name
         (_      , _    ) -> Small name

edge :: Parser Edge
edge =
  do v <- cave
     symbol "-"
     u <- cave
     return (v, u)

graph :: Parser Graph
graph =
  do es <- many edge
     eof
     return ((nub $ map fst es ++ map snd es) \\ [Start]
            , nub $ es ++ map (\(v, u) -> (u, v)) es)

isLarge :: Cave -> Bool
isLarge (Large _) = True
isLarge _         = False

paths :: Cave -> (Int, Path) -> Graph -> Int
paths End _ _ = 1
paths v  (joker, path) graph@(vertices, edges) =
  sum [ paths u (joker', v : path) graph
      | u       <-  vertices
      , (v, u) `elem` edges
      , joker' <- [if not (u `elem` path) || isLarge u then joker else joker - 1]
      , joker' >= 0
      ]

solution :: String -> IO ()
solution input =
  print $
  paths Start (0, []) <$>
  graph `from` input
