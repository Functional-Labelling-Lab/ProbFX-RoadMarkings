module Hough where
import CppFFI    (Line, Point)
import Data.List (sortOn)
import Data.Ord (comparing)
import GHC.IO (unsafePerformIO)

type LineCompError = Line -> Line -> Double

-- | Comparison of lines from Hough line detection:
-- The error for each predicted line is equal to the minimum area of the 
-- quadrilaterial traced between a line from the target image and the predicted
-- line, divided by the area of the image.
compareLines ::
  -- | The line comparison function to use
  LineCompError ->
  -- | Lines from the target image of form (w, p, θ)
  [Line] ->
  -- | Lines from the rendered scene
  [Line] ->
  -- | The image size (used to normalise)
  Point ->
  Double
compareLines errFun [] _ _= 1
compareLines errFun ls rs (h,w) 
  = sum (map (minimum . flip map ls . errFun) rs) / fromIntegral (h * w * length rs)

-- Using quadrilatierial area as a measure of error
quadError :: LineCompError
quadError (a,b) (c,d)
  = fromIntegral ((x1 * y2 + x2 * y3 + x3 * y4 + x4 * y1) -
                  (x2 * y1 + x3 * y2 + x4 * y3 + x1 * y4)) / 2
  where
    coords = [a,b,c,d]
    [(x1, y1),(x2, y2),(x3, y3),(x4, y4)] 
      = sortOn (antiClockwiseOrder $ center coords) coords
    
    -- Find the center of a collection of points
    center :: [Point] -> (Float, Float)
    center ps = (fromIntegral (sum xs) / len, fromIntegral (sum ys) / len)
      where
        len = fromIntegral $ length ps
        (xs, ys) =  unzip ps

    -- get the clockwise ordering of points around a center, split into 
    -- quadrants and compare normalised. We could also do this with tan 
    -- per quadrant
    antiClockwiseOrder :: (Float, Float) -> Point -> (Int, Float, Float)
    antiClockwiseOrder (cx, cy) (x,y)
      | dx >= 0 && dy <= 0 = (0,  dx,  dy)
      | dx >= 0 && dy  > 0 = (1, -dx,  dy)
      | dx  < 0 && dy >= 0 = (2, -dx, -dy)
      | dx  < 0 && dy  < 0 = (3,  dx, -dy)
      | otherwise          = error "unreachable"
      where
        (dx', dy') = (cx - fromIntegral x, cy - fromIntegral y)
        norm = sqrt (dx'^2 + dy'^2)
        -- must be normalised for comparison on dx, dy to work
        (dx, dy) = (dx' / norm, dy' / norm)

-- | Using the distance between line end points, the centers and the length of 
--   the lines.
compError :: LineCompError
compError (p1,p2) (q1,q2) 
  = dist (center p1 p2) (center q1 q2)
  + abs (dist p1 p2 - dist q1 q2)
  + min (dist p2 q1 + dist p1 q2) (dist p1 q1 + dist p2 q2)
  where
    dist :: Point -> Point -> Double
    dist (xa,ya) (xb,yb) = sqrt $ fromIntegral $ (xa - xb)^2 + (ya - yb)^2

    center :: Point -> Point -> Point
    center (xa,ya) (xb,yb) = ((xa + xb) `div` 2, (ya + yb) `div` 2)

