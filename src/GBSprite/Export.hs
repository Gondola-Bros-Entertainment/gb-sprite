-- | Export API for saving canvases to files.
--
-- Both BMP and PNG export are always available.
module GBSprite.Export
  ( -- * BMP
    exportBmp,

    -- * PNG
    exportPng,
  )
where

import GBSprite.BMP (writeBmp)
import GBSprite.Canvas (Canvas)
import GBSprite.PNG (writePng)

-- | Export a canvas as a BMP file.
exportBmp :: FilePath -> Canvas -> IO ()
exportBmp = writeBmp

-- | Export a canvas as a PNG file.
exportPng :: FilePath -> Canvas -> IO ()
exportPng = writePng
