module Origami where

import Data.Maybe
import Data.Ratio
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Debug.Trace
import Data.Tuple

data PointType = Point (Double, Double) deriving Show
data LineSegmentType = LineSegment (PointType, PointType) deriving Show
type Vector = (Double,Double)
data Origami = Origami { silhouette :: [[PointType]], skeleton :: [LineSegmentType], initialTransform :: (Ratio Integer,Ratio Integer) } deriving Show
type LineSegmentMap = Map.Map PointType [LineSegmentType]
type EndpointMap = Map.Map PointType [PointType]
data OrigamiSolution = OrigamiSolution { vertices :: [PointType], facets :: [[Int]], finalPositions :: [PointType] } deriving Show

epsilon = 0.0000001

-- Tests for two doubles being equal within a certain amount (epsilon)
(=~) :: Double -> Double -> Bool
(=~) x y = abs (x-y) < epsilon

-- Tests for two points being equal using =~
instance Eq PointType where
  (==) (Point (x1,y1)) (Point (x2,y2)) = (x1 =~ x2) && (y1 =~ y2)

-- Tests for two line segments being equal using =~ (via PointType ==)
instance Eq LineSegmentType where
  (==) (LineSegment (Point p11,Point p12)) (LineSegment (Point p21,Point p22)) =
    (p11 == p21) && (p12 == p22)

-- Allows points to be sorted
instance Ord PointType where
  compare (Point (x1,y1)) (Point (x2,y2)) =
    if (x1 =~ x2) then
      if (y1 =~ y2) then
        EQ
      else
        y1 `compare` y2
    else
      x1 `compare` x2

readInt :: String -> Int
readInt s = read s

readInteger :: String -> Integer
readInteger s = read s

-- Read a ratio of the form n/d (Haskell represents them as n%d) and
-- returns it as a Double
readRatio :: String -> Ratio Integer -> Double
readRatio s t =
  fromRational (if (length nums) == 1 then
    ((readInteger (head nums)) % 1) - t
  else
    (readInteger (head nums)) % (readInteger (head (tail nums))) - t)
      where
        nums = splitOn "/" s

-- Read a ratio of the form n/d and leave it as a ratio
readRational :: String -> Ratio Integer
readRational s =
  (if (length nums) == 1 then
    (readInteger (head nums)) % 1
  else
    (readInteger (head nums)) % (readInteger (head (tail nums))))
      where
        nums = splitOn "/" s

-- Parse a point specified as rational,rational
parsePoint :: (Ratio Integer, Ratio Integer) -> String -> PointType
parsePoint (tx, ty) s =
  Point (readRatio p1 tx, readRatio p2 tx)
    where
      [p1,p2] = splitOn "," s

-- Parse a line segment of the form x1,y1 x2,y2
parseLineSegment :: (Ratio Integer, Ratio Integer) -> String -> LineSegmentType
parseLineSegment t s =
  LineSegment (parsePoint t p1, parsePoint t p2)
    where
      [p1,p2] = splitOn " " s

-- Parse a silhouette from a list of strings
parseSilhouette :: Int -> (Ratio Integer, Ratio Integer) -> [String] -> [[PointType]] -> Origami
parseSilhouette 0 t (sn:ss) sil =
  Origami (reverse sil) (map (parseLineSegment t) (take n ss)) t
    where
      n = readInt sn

parseSilhouette n t (sn:ss) sil =
  parseSilhouette (n-1) t (drop pn ss) ((map (parsePoint t) (take pn ss)) : sil)
    where
      pn = readInt sn

-- Tries to find the minimum x and y values to figure out where to move
-- the shape to when normalizing
updateTrans :: (Maybe (Ratio Integer, Ratio Integer)) -> String -> Maybe (Ratio Integer, Ratio Integer)
updateTrans mt s =
  Just (newtx,newty)
    where
      [p1,p2] = splitOn "," s
      x = readRational p1
      y = readRational p2
      Just (tx,ty) = mt
      newtx = if (isNothing mt) || (x < tx) then x else tx
      newty = if (isNothing mt) || (y < ty) then y else ty

-- Loops through all the points to look for the minimum x and y values
parseInitialTransform :: Int -> [String] -> Maybe (Ratio Integer, Ratio Integer) -> Maybe (Ratio Integer, Ratio Integer)
parseInitialTransform 0 _ t = t
parseInitialTransform n (sn:ss) mt =
  parseInitialTransform (n-1) (drop pn ss) updatedTransform
    where
      updatedTransform = foldl' updateTrans mt (take pn ss)
      pn = readInt sn

-- Parse a problem specification file
parseOrigami :: String -> IO Origami
parseOrigami filename = do
  contents <- readFile filename
  let (ns:strs) = lines contents
  let n = readInt ns
  let (Just t) = parseInitialTransform n strs Nothing
  return $ parseSilhouette n t strs []

-- Given a list of line segments, return a mapping of endpoint to a list
-- of all points connected with that endpoint (both directions)
computeAdjacencyGraph :: [LineSegmentType] -> EndpointMap
computeAdjacencyGraph ls =
  foldl' addPoints Map.empty ls
    where
      addPoints m (LineSegment (p1,p2)) = Map.insertWith (++) p1 [p2] (Map.insertWith (++) p2 [p1] m)

fromLineSegment :: LineSegmentType -> Vector
fromLineSegment (LineSegment (Point (x1,y1),Point (x2,y2))) = (x2-x1,y2-y1)

vectorLen :: Vector -> Double
vectorLen (x,y) =
  sqrt ((x*x) + (y*y))

dotProduct :: Vector -> Vector -> Double
dotProduct (x1,y1) (x2,y2) =
  (x1*x2) + (y1*y2)

vectorAngle :: Vector -> Vector -> Double
vectorAngle v1 v2 = acos ((dotProduct v1 v2) / ((vectorLen v1) * (vectorLen v2)))

lineSegmentsAngle :: LineSegmentType -> LineSegmentType -> Double
lineSegmentsAngle l1 l2 = vectorAngle (fromLineSegment l1) (fromLineSegment l2)

-- Return the raw line segment angle
lineSegmentAngle :: LineSegmentType -> Double
lineSegmentAngle (LineSegment (Point (x1,y1),Point (x2,y2))) =
  if dist == 0 then
    0.0
  else if ydiff < 0 then
    -angle
  else
    angle
      where
        xdiff = x2 - x1
        ydiff = y2 - y1
        dist = sqrt ((xdiff*xdiff)+(ydiff*ydiff))
        angle = acos (xdiff / dist)

reverseLineSegment :: LineSegmentType -> LineSegmentType
reverseLineSegment (LineSegment (p1,p2)) = LineSegment (p2,p1)

point1 :: LineSegmentType -> PointType
point1 (LineSegment (p1,p2)) = p1

point2 :: LineSegmentType -> PointType
point2 (LineSegment (p1,p2)) = p2

px :: PointType -> Double
px (Point (x,y)) = x

py :: PointType -> Double
py (Point (x,y)) = y

-- Return the angle between line segments
cclLineSegmentsAngle :: LineSegmentType -> LineSegmentType -> Double
cclLineSegmentsAngle l1 l2 =
  if angleDiff < 0 then
    (angleDiff + (2.0 * pi))
  else if angleDiff > (2.0 * pi) then
    (angleDiff - (2.0 * pi))
  else
    angleDiff
      where
        angleDiff = pi - ((lineSegmentAngle l2) - (lineSegmentAngle l1))

lineSegmentLen :: LineSegmentType -> Double
lineSegmentLen l1 = vectorLen (fromLineSegment l1)

-- Reflects a point over a line segment
reflectPoint :: PointType -> LineSegmentType -> PointType
reflectPoint (Point (x,y)) (LineSegment (Point (x1,y1),Point (x2,y2))) =
  if (y1 == y2) then
    Point (x, y + (2 * (y1-y)))
  else if (x1 == x2) then
    Point (x + (2 * (x1 - x)), y)
  else
    Point (lx-(x-lx),ly-(y-ly))
      where
        m = (y2-y1)/(x2-x1)
        t = y1 - (m * x1)
        ms = -1.0 / m
        ts = y - (ms * x)
        lx = (ts-t) / (m - ms)
        ly = m * lx + t

-- Reflects a line segment over another (as if being folded)
reflectLineSegment :: LineSegmentType -> LineSegmentType -> LineSegmentType
reflectLineSegment (LineSegment (p1,p2)) ref =
  LineSegment ((reflectPoint p1 ref), (reflectPoint p2 ref))

segmentsJoined :: LineSegmentType -> LineSegmentType -> Bool
segmentsJoined (LineSegment (p11,p12)) (LineSegment (p21,p22)) =
   (p11 == p21) || (p11 == p22) || (p12 == p21) || (p12 == p22)

findIntersectingPair :: [LineSegmentType] -> Maybe (LineSegmentType,LineSegmentType)
findIntersectingPair [] = Nothing
findIntersectingPair [_] = Nothing
findIntersectingPair (l:ls) =
  if isJust foundSegment then
    Just (l,fromJust foundSegment)
  else
    findIntersectingPair ls
      where
        foundSegment = find (segmentsJoined l) ls
        segmentsJoined (LineSegment (p11,p12)) (LineSegment (p21,p22)) =
          (p11 == p21) || (p11 == p22) || (p12 == p21) || (p12 == p22)

findLargestPath' :: LineSegmentMap -> PointType -> LineSegmentType -> [PointType] -> [LineSegmentType] -> Maybe [LineSegmentType]
findLargestPath' m startPoint currSegment vertices segments =
  if isNothing justFarthest then
    Nothing
  else if farthestP2 == startPoint then
    Just (reverse (farthest:segments))
  else if elem farthestP2 vertices then
    Nothing
  else
    findLargestPath' m startPoint farthest (farthestP2:vertices) (farthest:segments)
      where
        justFarthest = getFarthest currSegment m
        farthest = fromJust justFarthest
        farthestP2 = point2 farthest

-- Finds the largest path in a list of line segments, which should be
-- the outline of the shape
findLargestPath :: [LineSegmentType] -> Maybe [LineSegmentType]
findLargestPath l =
  findLargestPath' (createLineSegmentMap l) startPoint startSegment startVertices startSegments
    where
      startSegment = maximumBy findMaxX l
      findMaxX (LineSegment (p11,p12)) (LineSegment (p21,p22)) =
        compare (max (px p11) (px p12)) (max (px p21) (px p22))
      startPoint = point1 startSegment
      startVertices = [point2 startSegment]
      startSegments = [startSegment]

corners :: [LineSegmentType] -> [LineSegmentType]
corners lines =
  if isJust justLargestPath then
    largestPath
  else
    lines
      where
        mergedLines = foldl' tryMergingSegs lines lines
        largestPath = fromJust justLargestPath
        justLargestPath = findLargestPath mergedLines

-- Is done is true if the largest path is a square - 4 line segements, all
-- length 1, at least one pair intersect at 90 degrees (if all the segments
-- are equal length and one pair is at 90 degrees, the others must be)
isDone :: [LineSegmentType] -> Bool
isDone lines =
  if (isJust justLargestPath) then
    if (length largestPath == 4) then
      if(all length1 largestPath) then
        if isJust joinedSegments then
          (lineSegmentsAngle joined1 joined2) =~ (pi / 2.0)
        else
          False
      else
        False
    else
      False
  else
    False
      where
        mergedLines = foldl' tryMergingSegs lines lines
        largestPath = fromJust justLargestPath
        justLargestPath = findLargestPath mergedLines
        joinedSegments = findIntersectingPair largestPath
        (joined1,joined2) = fromJust joinedSegments
        length1 l = (lineSegmentLen l) =~ 1.0

-- When a line segment intersects multiple segments, find the one that
-- intersects at the smallest angle
getClosest :: LineSegmentType -> LineSegmentMap -> Maybe LineSegmentType
getClosest l m =
  if (length filtered) > 0 then
    Just $ minimumBy compareAngles filtered
  else
    Nothing
    where
      compareAngles l1 l2 = compare (cclLineSegmentsAngle l l1) (cclLineSegmentsAngle l l2)
      notReflection l1 = not (l == (reverseLineSegment l1))
      filtered = filter notReflection (m Map.! (point2 l))

-- When a line segment intersects multiple segments, find the one that
-- intersects at the largest angle
getFarthest :: LineSegmentType -> LineSegmentMap -> Maybe LineSegmentType
getFarthest l m =
  if (length filtered) > 0 then
    Just $ maximumBy compareAngles filtered
  else
    Nothing
    where
      compareAngles l1 l2 = compare (cclLineSegmentsAngle l l1) (cclLineSegmentsAngle l l2)
      notReflection l1 = not (l == (reverseLineSegment l1))
      filtered = filter notReflection (m Map.! (point2 l))

removeSegmentsFromMap :: [LineSegmentType] -> Map.Map PointType Int -> Map.Map PointType Int
removeSegmentsFromMap [] m = m
removeSegmentsFromMap (l:ls) m =
  removeSegmentsFromMap ls (Map.adjust decrementPoint (point1 l) m)
    where
      decrementPoint p = p-1

-- Takes a list of line segments and creates a mapping from an
-- endpoint to all the line segments that have an endpoint at that point
createLineSegmentMap :: [LineSegmentType] -> LineSegmentMap
createLineSegmentMap lines = foldl' addLineSegment Map.empty lines
  where
  addLineSegment m (LineSegment (p1,p2)) = Map.insertWith (++) p1 [LineSegment (p1,p2)]
    (Map.insertWith (\x y -> nub $ x ++ y) p2 [LineSegment (p2,p1)] m)

-- Looks for the smallest shape that this line segment is a part of
findShape :: LineSegmentMap -> Map.Map PointType Int -> PointType -> LineSegmentType -> [PointType] -> [LineSegmentType] -> Maybe ([LineSegmentType], LineSegmentMap, Map.Map PointType Int)
findShape m pc startPoint currSegment vertices segments =
  if isNothing justClosest then
    Nothing
  else if closestP2 == startPoint then
    Just (reverse (closest:segments), m, removeSegmentsFromMap (closest:segments) pc)
  else if elem closestP2 vertices then
    Nothing
  else
    findShape m pc startPoint closest (closestP2:vertices) (closest:segments)
      where
        closest = fromJust justClosest
        justClosest = getClosest currSegment m
        closestP2 = point2 closest

getShapes'' :: LineSegmentMap -> Map.Map PointType Int -> [LineSegmentType] -> [[LineSegmentType]] -> Maybe ([[LineSegmentType]], LineSegmentMap, Map.Map PointType Int)
getShapes'' m pc [] lss = Nothing
getShapes'' m pc (l:ls) lss =
  if isJust foundShape then
    Just ((foundShapeLS : lss),foundShapeM, foundShapePC)
  else
    getShapes'' m pc ls lss
      where
        foundShape = findShape m pc (point1 l) l [point2 l] [l]
        (foundShapeLS,foundShapeM,foundShapePC) = fromJust foundShape

getShapes' :: LineSegmentMap -> Map.Map PointType Int -> [PointType] -> [[LineSegmentType]] -> [[LineSegmentType]]
getShapes' m pc [] lss = lss
getShapes' m pc (p:ps) lss =
  if isJust foundShape then
    getShapes' foundShapeM foundShapePC startingPoints foundShapeLS
  else
    getShapes' m pc ps lss
      where
        foundShape = getShapes'' m pc (m Map.! p) lss
        (foundShapeLS,foundShapeM,foundShapePC) = fromJust foundShape
        startingPoints = filter (\k -> (foundShapePC Map.! k) > 1) (Map.keys foundShapeM)

-- Takes a list of line segments and returns a list of shapes formed by
-- the segments (I don't think this actually works the way it should)
getShapes :: [LineSegmentType] -> [[LineSegmentType]]
getShapes lines = getShapes' lineSegmentMap pointCount (Map.keys lineSegmentMap) []
  where
    pointCount = Map.map length lineSegmentMap
    lineSegmentMap = createLineSegmentMap lines

-- When folding over a segment, look for the smallest shape that the
-- segment is a part of
findSmallestFold :: LineSegmentMap -> PointType -> LineSegmentType -> [PointType] -> [LineSegmentType] -> Maybe [LineSegmentType]
findSmallestFold m startPoint currSegment vertices segments =
  if isNothing justClosest then
    Nothing
  else if closestP2 == startPoint then
    Just (reverse (closest:segments))
  else if elem closestP2 vertices then
    Nothing
  else
    findSmallestFold m startPoint closest (closestP2:vertices) (closest:segments)
      where
        justClosest = getClosest currSegment m
        closest = fromJust justClosest
        closestP2 = point2 closest

-- Given a line segment, return the segments that would be changed by
-- a fold over the line segment
getSegmentsToFold :: LineSegmentType -> [LineSegmentType] -> Maybe [LineSegmentType]
getSegmentsToFold ref segs =
  if isJust smallestFold then
    Just (removeRef ref (fromJust smallestFold))
  else if isJust smallestFoldRev then
    Just (removeRef reverseRef (fromJust smallestFoldRev))
  else
    Nothing
      where
        smallestFold = findSmallestFold (createLineSegmentMap segs) (point1 ref) ref [point2 ref] [ref]
        smallestFoldRev = findSmallestFold (createLineSegmentMap segs) (point1 reverseRef) reverseRef [point2 reverseRef] [reverseRef]
        reverseRef = LineSegment (point2 ref,point1 ref)
        removeRef ref segs = delete ref segs

-- Return true of two segments share an endpoint and the segments lie
-- along the same line
hasMatchingEndpoint newSeg (LineSegment (mp1,mp2)) = ((LineSegment (mp1,mp2)) /= newSeg) && ((LineSegment (mp2,mp1)) /= newSeg) && ((np1==mp1) || (np1==mp2) || (np2==mp1) || (np2==mp2)) &&
      (((lineSegmentAngle newSeg) =~ (lineSegmentAngle (LineSegment (mp1,mp2)))) ||
       ((lineSegmentAngle newSeg) =~ (lineSegmentAngle (LineSegment (mp2,mp1)))))
  where
    LineSegment (np1,np2) = newSeg

-- Joins two adjacent segments into one (this is only useful if the
-- segments lie along the same line)
joinedSeg (LineSegment (np1,np2)) (LineSegment (p1,p2)) =
  if (np1==p1) then LineSegment (np2,p2)
  else if (np1==p2) then LineSegment (np2,p1)
  else if (np2==p1) then LineSegment (np1,p2)
  else LineSegment (np1,p1)

-- Merge any adjacent segments that lie along the same line (i.e. when
-- you unfold, one of the unfolded segments may have been part of a
-- longer line when it was folded, and this recovers that line)
tryMergingSegs :: [LineSegmentType] -> LineSegmentType -> [LineSegmentType]
tryMergingSegs ls newSeg =
  if lineSegmentLen newSeg > 1.5 then
    ls
  else if notElem newSeg ls then
    ls
  else if isJust foundSeg then
    tryMergingSegs (joined:reducedSegs) joined
  else
    ls
  where
    joined = joinedSeg newSeg (fromJust foundSeg)
    foundSeg = find (hasMatchingEndpoint newSeg) ls
    reducedSegs = delete (fromJust foundSeg) (delete (reverseLineSegment (fromJust foundSeg))
      (delete newSeg (delete (reverseLineSegment newSeg) ls)))

-- Reflects segments over the specified segment to reflect over
updateData :: LineSegmentType -> ([LineSegmentType], Map.Map PointType Int, Map.Map Int Int, [LineSegmentType]) -> LineSegmentType -> ([LineSegmentType], Map.Map PointType Int, Map.Map Int Int, [LineSegmentType])
updateData ref (segs,m,origMap,ls) l =
  (updatedSegs, updatedMap, updatedOrigMap,newls)
    where
      reflectedSeg = reflectLineSegment l ref
      LineSegment (p1,p2) = reflectedSeg
      LineSegment (op1,op2) = l
      updatedMap = updatePoint p1 (updatePoint p2 m)
      updatePoint p m = if Map.member p m then m else Map.insert p (Map.size m) m
      updatedOrigMap = updateOrigPoint p1 op1 (updateOrigPoint p2 op2 origMap)
      updateOrigPoint p op = Map.insert (updatedMap Map.! p) (origMap Map.! (m Map.! op))
      updatedSegs = nub $ if notElem reflectedSeg segs then (reflectedSeg:segs) else segs
      newls = tryMergingSegs ls reflectedSeg

-- Folds line segments over another line segment
foldOverSegment :: LineSegmentType -> [LineSegmentType] -> Map.Map PointType Int -> Map.Map Int Int -> [LineSegmentType] -> Maybe ([LineSegmentType], Map.Map PointType Int, Map.Map Int Int, [LineSegmentType])
foldOverSegment l segs m origMap ls =
  if isJust foldSegments then
    Just (foldl' (updateData l) (segs,m,origMap,ls) (fromJust foldSegments))
  else
    Nothing
      where
        foldSegments = getSegmentsToFold l segs

-- Determine whether the overall unfolded silhouette is impossibly large
tooBig :: Map.Map PointType Int -> Bool
tooBig m = (((maxX-minX)*(maxX-minX)) + ((maxY-minY)*(maxY-minY))) > 2
  where
    minX = minimum $ map px (Map.keys m)
    maxX = maximum $ map px (Map.keys m)
    minY = minimum $ map py (Map.keys m)
    maxY = maximum $ map py (Map.keys m)

tryUnfold' :: [LineSegmentType] -> [LineSegmentType] -> Map.Map PointType Int -> Map.Map Int Int -> Maybe ([LineSegmentType], Map.Map PointType Int, Map.Map Int Int, [LineSegmentType])
tryUnfold' [] segs m origMap = if isDone segs then Just (segs,m,origMap,corners segs) else Nothing
tryUnfold' (l:ls) segs m origMap =
  if isDone segs then
    Just (segs,m,origMap,corners segs)
  else if tooBig m then
    Nothing
  else if isJust tryFoldover then
    tryUnfold' newls (l:foldedSegs) foldedPointMap foldedOrigMap
  else
    Nothing
      where
        (foldedSegs,foldedPointMap,foldedOrigMap,newls) = fromJust tryFoldover
        tryFoldover = foldOverSegment l segs m origMap ls

-- Given a permutation of line segments, try unfolding over those segments
-- in order until either a 1x1 square is reached or the shape is
-- impossibly large, or we run out of segments
tryUnfold :: [[LineSegmentType]] -> [LineSegmentType] -> Map.Map PointType Int -> Map.Map Int Int -> Maybe ([LineSegmentType], Map.Map PointType Int, Map.Map Int Int, [LineSegmentType])
tryUnfold [] segs m origMap = if isDone segs then Just (segs,m,origMap,corners segs) else Nothing
tryUnfold (ls:lss) segs m origMap =
  if isDone segs then
    Just (segs,m,origMap,corners segs)
  else if isJust unfolded then
    unfolded
  else
    tryUnfold lss segs m origMap
      where
        unfolded = tryUnfold' ls segs m origMap

-- Normalize the line segments to a square at 0,0  1,0  1,1  0,1
normalize :: [LineSegmentType] -> PointType -> PointType
normalize corners (Point (x,y)) =
  Point (newX,newY)
  where
    normalAngle = minimum $ map lineSegmentAngle corners
    cornerPoints = map point1 corners
    centerX = (sum $ map px cornerPoints) /4.0
    centerY = (sum $ map py cornerPoints) /4.0
    centerLine = LineSegment (Point (centerX,centerY), Point (x,y))
    centerLineLen = lineSegmentLen centerLine
    currAngle = lineSegmentAngle centerLine
    newX = 0.5 + centerLineLen * (cos (currAngle - normalAngle))
    newY = 0.5 + centerLineLen * (sin (currAngle - normalAngle))

normalizeLineSegment :: [LineSegmentType] -> LineSegmentType -> LineSegmentType
normalizeLineSegment corners (LineSegment (p1,p2)) =
  LineSegment (normalize corners p1, normalize corners p2)

unfold :: Origami -> Maybe OrigamiSolution
unfold (Origami sil sk t) =
  if isJust unfolded then
    Just (OrigamiSolution pointList shapes finalPoints)
  else
    Nothing
  where
    unfolded = tryUnfold (permutations sk) sk startPointMap startOrigPointMap
    (unfoldedSegs,unfoldedPointMap,unfoldedOrigPointMap,unfoldedCorners) = fromJust unfolded
    lineSegmentMap = createLineSegmentMap sk
    startPointMap = Map.fromList $ zip (Map.keys lineSegmentMap) [0..]
    startOrigPointMap = Map.fromList $ zip (Map.elems startPointMap) (Map.elems startPointMap)
    pointList = map (normalize unfoldedCorners) (map ((Map.!) reversePointMap) (sort (Map.keys reversePointMap)))
    reversePointMap = Map.fromList (map swap (Map.toList unfoldedPointMap))
    reverseStartPointMap = Map.fromList (map swap (Map.toList startPointMap))
    foundShapes = getShapes $ foldl' tryMergingSegs unfoldedSegs unfoldedSegs
    shapes = nub $ map createShapePointList foundShapes
    createShapePointList segList =  map ((Map.!) unfoldedPointMap) (map point1 segList)
    finalPoints = map (\n -> reverseStartPointMap Map.! (unfoldedOrigPointMap Map.! n)) (sort (nub (Map.elems unfoldedPointMap)))
