{-# LANGUAGE CPP #-}

-- | Unified export API.
--
-- Always available: BMP export.
-- With @juicy-pixels@ flag: PNG export via JuicyPixels.
module GBSprite.Export
  ( -- * BMP (always available)
    exportBmp,

    -- * PNG (requires juicy-pixels flag)
#ifdef JUICY_PIXELS
    exportPng,
#endif
  )
where

import GBSprite.BMP (writeBmp)
import GBSprite.Canvas (Canvas)

#ifdef JUICY_PIXELS
import Codec.Picture (writePng, Image(..), PixelRGBA8(..))
import qualified Data.Vector.Storable as VS
import GBSprite.Canvas (Canvas(..))
#endif

-- | Export a canvas as a BMP file.
exportBmp :: FilePath -> Canvas -> IO ()
exportBmp = writeBmp

#ifdef JUICY_PIXELS
-- | Export a canvas as a PNG file (requires @juicy-pixels@ flag).
exportPng :: FilePath -> Canvas -> IO ()
exportPng path (Canvas w h pixels) =
  let img = Image w h (VS.map id pixels) :: Image PixelRGBA8
   in writePng path img
#endif
