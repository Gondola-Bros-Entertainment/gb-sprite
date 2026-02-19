-- | Export API for saving canvases to files.
--
-- BMP export is always available. For PNG export, enable the
-- @juicy-pixels@ cabal flag and import "GBSprite.Export.PNG".
module GBSprite.Export
  ( -- * BMP (always available)
    exportBmp,
  )
where

import GBSprite.BMP (writeBmp)
import GBSprite.Canvas (Canvas)

-- | Export a canvas as a BMP file.
exportBmp :: FilePath -> Canvas -> IO ()
exportBmp = writeBmp
