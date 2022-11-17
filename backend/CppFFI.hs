{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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
              , getSceneFBO) where

import GHC.Generics (Generic(..))
import Foreign (Storable(..), Ptr(..))
import Foreign.CStorable (CStorable(..))
import Foreign.C.Types (CChar, CInt(..), CUInt(..))
import Foreign.C.String
import Foreign

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

data TextureFBO = TextureFBO
    { texture :: Texture
    , frameBuffer :: FrameBuffer
    } deriving (Generic, CStorable, Show)

instance Storable TextureFBO where
    sizeOf = cSizeOf
    alignment = cAlignment
    poke = cPoke
    peek = cPeek

instance Storable Camera where
    peek      = cPeek
    poke      = cPoke
    alignment = cAlignment
    sizeOf    = cSizeOf

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

foreign import ccall unsafe "render_scene_c" renderScene :: Ptr Scene -> FrameBuffer -> IO ()
foreign import ccall unsafe "set_target_img_c" setTargetImg' :: CString -> IO ()
foreign import ccall unsafe "test_bed_c" testBed :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int32
foreign import ccall unsafe "get_mean_pixel_value_c" getMeanPixelValue :: IO Double
foreign import ccall unsafe "find_texture_difference_c" findTextureDifference :: IO ()
foreign import ccall unsafe "hough_lines_c" rawHoughLines :: CString -> IO (Ptr DetectedLines)

foreign import ccall unsafe "create_texture_fbo_c" createTextureFBO' :: IO (Ptr TextureFBO)
foreign import ccall unsafe "get_scene_fbo_c" getSceneFBO :: IO FrameBuffer
foreign import ccall unsafe "get_target_texture_c" getTargetTexture :: IO Texture

-- By default Haskell's free uses C's free, but we use C's directly for a guarantee
foreign import ccall "stdlib.h free" c_free :: Ptr a -> IO ()

getHoughLines :: String -> [Line]
getHoughLines path = unsafePerformIO $ withCString path (\c_path ->
    do
        -- Access the detected_lines struct and pointer
        rawPtr <- rawHoughLines c_path
        raw <- peek rawPtr

        -- Read the array of hough_lines structs and extract
        houghLines <- peekArray (fromIntegral $ len raw) (hlines raw)
        let lines = map extractHoughLines houghLines

        -- Clean up
        c_free (hlines raw)
        c_free rawPtr

        return lines
    )

-- sets context->targetTexture to image in path
setTargetImg :: String -> IO ()
setTargetImg s = newCString s >>= setTargetImg'

createTextureFBO :: IO TextureFBO
createTextureFBO = createTextureFBO' >>= peek
