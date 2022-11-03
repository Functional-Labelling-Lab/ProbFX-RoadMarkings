{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ForeignFunctionInterface #-}



module MyLib (Scene(..), Camera(..), renderScene, setTargetImg, testBed, getMeanPixelValue, findTextureDifference) where

import GHC.Generics (Generic(..))
import Foreign (Storable(..), Ptr(..))
import Foreign.CStorable (CStorable(..))
import Foreign.C.Types (CChar, CInt)
import Foreign.C.String
import Foreign

data Camera = Camera
    { x :: Double
    , y :: Double
    , z :: Double
    , pitch :: Double
    , yaw :: Double
    , roll :: Double
    } deriving (Generic, CStorable, Show)

instance Storable Camera where
    peek      = cPeek
    poke      = cPoke
    alignment = cAlignment
    sizeOf    = cSizeOf

data Scene = Scene
    { roadWidth :: Double
    , camera :: Camera
    } deriving (Generic, CStorable, Show)

instance Storable Scene where
    peek      = cPeek
    poke      = cPoke
    alignment = cAlignment
    sizeOf    = cSizeOf

foreign import ccall unsafe "render_scene" renderScene :: Ptr Scene -> IO ()
foreign import ccall unsafe "set_target_img" setTargetImg :: CString -> IO ()
foreign import ccall unsafe "test_bed" testBed :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int32
foreign import ccall unsafe "get_mean_pixel_value" getMeanPixelValue :: IO Double
foreign import ccall unsafe "find_texture_difference" findTextureDifference :: IO ()