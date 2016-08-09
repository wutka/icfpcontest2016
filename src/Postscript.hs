module Postscript where

import Data.Ratio
import Origami

ratioToPSInt :: Double -> Int
ratioToPSInt r = floor (150.0 * r)

pointToPS :: PointType -> String
pointToPS (x,y) = (show $ ratioToPSInt x) ++ " " ++ (show $ 600 + ratioToPSInt y)

pointOffsetToPS :: Double -> PointType -> String
pointOffsetToPS xoffset (x,y) = (show $ ratioToPSInt (xoffset + x)) ++ " " ++ (show $ 600 + ratioToPSInt y)

generateSkeletonPS :: Double -> [LineSegmentType] -> [String]
generateSkeletonPS xoffset lines =
  concatMap generateSkeletonLine lines
    where
      generateSkeletonLine (p1,p2) =
        [ "0.1 setlinewidth", (pointOffsetToPS xoffset p1) ++ " newpath moveto"] ++
          [(pointOffsetToPS xoffset p2) ++ " lineto", "closepath", "0 setgray", "stroke" ]

generateBoxPS :: Double -> [String]
generateBoxPS offset =
  [ "0.1 setlinewidth", (show $ ratioToPSInt offset) ++ " 600 newpath moveto",
    (pointOffsetToPS offset (1.0, 0.0)) ++ " lineto",
    (pointOffsetToPS offset (1.0, 1.0)) ++ " lineto",
    (pointOffsetToPS offset (0.0, 1.0)) ++ " lineto",
    (show $ ratioToPSInt offset) ++ " 600 lineto", "closepath", "0 setgray", "stroke" ]

generatePolygonPS :: Double -> [PointType] -> [String]
generatePolygonPS xoffset (f:r) =
  [ "0.1 setlinewidth", (pointOffsetToPS xoffset f) ++ " newpath moveto"] ++
    (map makeLine r) ++ [(pointOffsetToPS xoffset f) ++ " lineto", "closepath", "0 setgray", "stroke" ]
      where
        makeLine p = (pointOffsetToPS xoffset p) ++ " lineto"

generateSilhouettePS :: Double -> [[PointType]] -> [String]
generateSilhouettePS xoffset [] = []
generateSilhouettePS xoffset (p:ps) =
  (generatePolygonPS xoffset p) ++ (generateSilhouettePS xoffset ps)

generateOrigamiPS :: Origami -> String
generateOrigamiPS (Origami sil skel) =
  unlines $ ["#!PS"] ++ (generateBoxPS (0.0)) ++ (generateSilhouettePS (0.0) sil) ++
    (generateBoxPS offset) ++ (generateSkeletonPS offset skel) ++ ["showpage"]
      where
        offset = (3.0)
