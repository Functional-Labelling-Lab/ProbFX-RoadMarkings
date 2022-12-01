{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module CppFFI ( Scene(..)
              , Camera(..)
              , renderScene
              , setTargetImg
              , testBed
              , getMeanPixelValue
              , findTextureDifference
              , createTextureFBO
              , TextureFBO(..)
              , GLuint
              , Texture
              , FrameBuffer
              , getSceneFBO
              , getTargetTexture
              , Line
              , Point
              , getHoughLines
              , getSceneLines
              , screenBuffer) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types   (CChar, CInt, CUInt(..))
import           Foreign.CStorable (CStorable (..))
import           GHC.Generics      (Generic (..))
import           GHC.IO            (unsafePerformIO)

type GLuint = CUInt
type Texture = GLuint
type FrameBuffer = GLuint

data Camera = Camera
  { x     :: Double
  , y     :: Double
  , z     :: Double
  , pitch :: Double
  , yaw   :: Double
  , roll  :: Double
  } deriving (Generic, CStorable, Show)

instance Storable Camera where
  peek      = cPeek
  poke      = cPoke
  alignment = cAlignment
  sizeOf    = cSizeOf

data TextureFBO = TextureFBO
    { texture :: Texture
    , frameBuffer :: FrameBuffer
    } deriving (Generic, CStorable, Show)

instance Storable TextureFBO where
    sizeOf    = cSizeOf
    alignment = cAlignment
    poke      = cPoke
    peek      = cPeek

data Scene = Scene
    { camera :: Camera
    } deriving (Generic, CStorable, Show)

instance Storable Scene where
  peek      = cPeek
  poke      = cPoke
  alignment = cAlignment
  sizeOf    = cSizeOf

type Point = (Int, Int)
type Line = (Point, Point)

screenBuffer :: FrameBuffer
screenBuffer = 0

extractHoughLines :: HoughLine -> Line
extractHoughLines (HoughLine sx sy ex ey)
    = ((fromIntegral sx, fromIntegral sy), (fromIntegral ex, fromIntegral ey))

data HoughLine = HoughLine {
  startX :: CInt,
  startY :: CInt,
  endX   :: CInt,
  endY   :: CInt
} deriving (Generic, CStorable, Show)

instance Storable HoughLine where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

data DetectedLines = DetectedLines {
    hlines :: Ptr HoughLine,
    len    :: CInt
} deriving (Generic, CStorable, Show)

instance Storable DetectedLines where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

-- Test bed for demoing pipeline
foreign import ccall unsafe "test_bed_c" rawTestBed :: CString -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int32
testBed :: String -> Scene -> IO Int32
testBed s Scene {camera=scene}
  = do
    s' <- newCString s
    rawTestBed s' (x scene) (y scene) (z scene) (pitch scene) (yaw scene) (roll scene)

-- Rendering a scene to a framebuffer
foreign import ccall unsafe "render_scene_c" renderScene :: Ptr Scene -> FrameBuffer -> IO ()

-- Setting the target image
foreign import ccall unsafe "set_target_img_c" rawSetTargetImg :: CString -> IO ()
setTargetImg :: String -> IO ()
setTargetImg s = newCString s >>= rawSetTargetImg

-- Get the mean pixel value
foreign import ccall unsafe "get_mean_pixel_value_c" getMeanPixelValue :: Int32 -> IO Double
foreign import ccall unsafe "find_texture_difference_c" findTextureDifference :: Int32 -> IO ()
foreign import ccall unsafe "scene_lines_c" rawSceneLines :: Ptr Scene -> IO (Ptr DetectedLines)

getSceneLines :: Scene -> [Line]
getSceneLines s = unsafePerformIO $ do
  -- Create pointer to scene
  scene <- malloc
  poke scene s

  -- Read the array of lines and extract
  rawPtr <- rawSceneLines scene
  raw <- peek rawPtr

  sceneLines <- peekArray (fromIntegral $ len raw) (hlines raw)
  let lines = map extractHoughLines sceneLines

  -- Clean up
  if fromIntegral (len raw) /= 0
    then do c_free (hlines raw)
    else do return ()

  c_free rawPtr
  free scene

  return lines

-- Get the lines from an image using the probabilistic hough transform
foreign import ccall unsafe "hough_lines_c" rawHoughLines :: CString -> IO (Ptr DetectedLines)
getHoughLines :: String -> [Line]
getHoughLines path = unsafePerformIO $ withCString path $ \c_path ->
  do
    -- Access the detected_lines struct and pointer
    rawPtr <- rawHoughLines c_path
    raw    <- peek rawPtr

    -- Read the array of hough_lines structs and extract
    houghLines <- peekArray (fromIntegral $ len raw) (hlines raw)
    let lines = map extractHoughLines houghLines

    -- Clean up
    c_free (hlines raw)
    c_free rawPtr

    return lines

-- By default Haskell's free uses C's free, but we use C's directly for a guarantee
foreign import ccall "stdlib.h free" c_free :: Ptr a -> IO ()

-- TODO: upgrade for channels
foreign import ccall unsafe "create_texture_fbo_c" rawCreateTextureFBO :: IO (Ptr TextureFBO)
createTextureFBO :: IO TextureFBO
createTextureFBO = rawCreateTextureFBO >>= peek

-- TODO: upgrade for channels
foreign import ccall unsafe "get_scene_fbo_c" getSceneFBO :: IO FrameBuffer

-- TODO: upgrade for channels
foreign import ccall unsafe "get_target_texture_c" getTargetTexture :: IO Texture
