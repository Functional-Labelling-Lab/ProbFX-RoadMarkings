module Hough where
import           CppFFI    (Line, Point)
import Data.List (sortOn)
import Data.Ord (comparing)

type Error = Float

-- | Comparison of lines from Hough line detection:
-- Uses the following algorithm:
-- 1. Filters for weight larger than the minimum
-- 2. For each line from the rendered scene:
compareLines ::
  -- | Lines from the target image of form (w, p, θ)
  [Line] ->
  -- | Lines from the rendered scene
  [Line] ->
  -- | The image size (used to normalise)
  Point ->
  Error
compareLines [] _ _= 1
compareLines ls rs (h,w) = sum (map (minimum . flip map ls . quadArea) rs) / fromIntegral (h * w * length rs)
  where
    -- Get the area of the quadrilateral traced between the two lines.
    quadArea :: Line -> Line -> Float
    quadArea (a,b) (c,d)
      = fromIntegral ((x1 * y2 + x2 * y3 + x3 * y4 + x4 * y1) -
                      (x2 * y1 + x3 * y2 + x4 * y3 + x1 * y4)) / 2
      where
        coords = [a,b,c,d]
        [(x1, y1),(x2, y2),(x3, y3),(x4, y4)] = sortOn (clockwiseOrder $ center coords) coords

    -- Find the center of a collection of points
    center :: [Point] -> (Float, Float)
    center ps = (fromIntegral (sum xs) / len, fromIntegral (sum ys) / len)
      where
        len = fromIntegral $ length ps
        (xs, ys) =  unzip ps

    -- get the clockwise ordering of points around a center, split into 
    -- quadrants and compare normalised. We could also do this with tan 
    -- per quadrant
    clockwiseOrder :: (Float, Float) -> Point -> (Int, Float, Float)
    clockwiseOrder (cx, cy) (x,y)
      | dx >= 0 && dy <= 0 = (0,  dx,  dy)
      | dx >= 0 && dy  > 0 = (1, -dx,  dy)
      | dx  < 0 && dy >= 0 = (2, -dx, -dy)
      | dx  < 0 && dy  < 0 = (3,  dx, -dy)
      where
        (dx', dy') = (cx - fromIntegral x, cy - fromIntegral y)
        norm = sqrt (dx'^2 + dy'^2)
        -- must be normalised for comparison on dx, dy to work
        (dx, dy) = (dx' / norm, dy' / norm)
