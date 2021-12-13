module Year2021.Day13.Puzzle2 where

import AdventLib.Grids
import AdventLib.Parsing
import Year2021.Day13.Puzzle1
import Control.Monad ( void  )

type Paper = Grid Char

paper :: [Point] -> Paper
paper dots = updates (const '#') dots $ uncurry fresh (contains dots) '.'

onPaper :: Traversable t => t [Point] -> IO ()
onPaper = void . mapM (mapM putStrLn) . fmap paper

solution :: String -> IO ()
solution input =
  onPaper $
  uncurry solve <$>
  transparant `from` input
