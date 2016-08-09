module Main where

import System.Environment
import Origami
import Data.Ratio
import Data.Maybe
--import Postscript

printSolution :: OrigamiSolution -> (Ratio Integer, Ratio Integer) -> IO ()
printSolution (OrigamiSolution vertices facets finalPositions) (tx,ty) = do
  putStrLn $ show $ length vertices
  putStr $ unlines $ map makeVertexString vertices
  putStrLn $ show $ length facets
  putStr $ unlines $ map makeFacetString facets
  putStr $ unlines $ map makeVertexStringTx finalPositions
    where
      makeVertexString (Point (x,y)) = showRational ((approxRational x 0.0000001))++","++(showRational ((approxRational y 0.0000001)))
      makeVertexStringTx (Point (x,y)) = showRational ((approxRational x 0.0000001)+tx)++","++(showRational ((approxRational y 0.0000001)+ty))
      showRational r = if (denominator r) == 1 then
                          show $ numerator r
                       else
                          (show $ numerator r)++"/"++(show $ denominator r)
      makeFacetString l = (show $ length l) ++ (concatMap (\i -> " "++(show i)) l)

main :: IO ()
main = do
  args <- getArgs
  origami <- parseOrigami (head args)
  let unfolded = unfold origami
  printSolution (fromJust unfolded) (initialTransform origami)
--  putStrLn $ generateOrigamiPS origami
