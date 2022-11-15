module Hough where
import Data.List (sort)

type Point = (Int, Int)

type Error = Float

type HoughLine = (Point, Point)

-- |
inputHoughLines ::
  -- | Input image path
  String ->
  -- | Rho, theta and number of votes for each line
  [HoughLine]
inputHoughLines = undefined

-- | Comparison of lines from Hough line detection:
-- Uses the following algorithm:
-- 1. Filters for weight larger than the minimum
-- 2. For each line from the rendered scene:
--   a. Get the quadrilaterial area (traced out by both segments)
--   b. Take the 
compareLines ::
  -- | Lines from the target image of form (w, p, θ)
  [HoughLine] ->
  -- | Lines from the rendered scene
  [HoughLine] ->
  -- | The image size (used to normalise)
  Point -> 
  Error
compareLines [] _ _= 1
compareLines ls rs (h,w) = sum (map (minimum . flip map ls . quadErr) rs) / fromIntegral (h * w)
  where
    quadErr :: HoughLine -> HoughLine -> Float
    quadErr (a,b) (c,d)
      = fromIntegral ((x1 * y2 + x2 * y3 + x3 * y4 + x4 * y1) -
                      (x2 * y1 + x3 * y2 + x4 * y3 + x1 * y4)) / 2
      where
        [(x1, y1), (x2, y2),(x3, y3), (x4, y4)] = sort [a,b,c,d]
