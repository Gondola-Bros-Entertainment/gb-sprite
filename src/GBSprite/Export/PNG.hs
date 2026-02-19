-- | PNG export via JuicyPixels.
--
-- This module is only available when the @juicy-pixels@ cabal flag
-- is enabled.
module GBSprite.Export.PNG
  ( exportPng,
  )
where

import Codec.Picture (Image (..), PixelRGBA8 (..), writePng)
import qualified Data.Vector.Storable as VS
import GBSprite.Canvas (Canvas (..))

-- | Export a canvas as a PNG file.
--
-- Requires the @juicy-pixels@ flag to be enabled.
exportPng :: FilePath -> Canvas -> IO ()
exportPng path (Canvas w h pixels) =
  let img = Image w h (VS.map id pixels) :: Image PixelRGBA8
   in writePng path img
