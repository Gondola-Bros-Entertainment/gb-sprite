-- | File I\/O for saving and loading canvases.
--
-- This is the only module (together with "GBSprite.Import") that
-- performs I\/O. Every other module in the library is pure.
module GBSprite.Export
  ( -- * BMP
    exportBmp,
    writeBmp,

    -- * PNG
    exportPng,
    writePng,
  )
where

import qualified Data.ByteString.Lazy as BL
import GBSprite.BMP (encodeBmp)
import GBSprite.Canvas (Canvas)
import GBSprite.PNG (encodePng)

-- | Export a canvas as a BMP file.
exportBmp :: FilePath -> Canvas -> IO ()
exportBmp path canvas = BL.writeFile path (encodeBmp canvas)

-- | Export a canvas as a PNG file.
exportPng :: FilePath -> Canvas -> IO ()
exportPng path canvas = BL.writeFile path (encodePng canvas)

-- | Alias for 'exportBmp'.
writeBmp :: FilePath -> Canvas -> IO ()
writeBmp = exportBmp

-- | Alias for 'exportPng'.
writePng :: FilePath -> Canvas -> IO ()
writePng = exportPng
