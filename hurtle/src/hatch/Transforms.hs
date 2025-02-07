--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

module Transforms where

--------------------------------------------------------------------------------

import Layout
import qualified Graphics.Gloss as Gloss

--------------------------------------------------------------------------------

-- Rotate an image in degrees.
-- Wraps the Gloss Picture rotation.
rotate :: Int -> Image -> Image
rotate r = fmap (Gloss.rotate $ fromIntegral r)

offset :: Float -> Float -> Image -> Image
offset x y = fmap $ Gloss.translate (x) (y)

scale :: Float -> Image -> Image
scale s = fmap $ Gloss.scale s s

mirror :: Image -> Image
mirror = fmap $ Gloss.scale (-1) 1

--------------------------------------------------------------------------------
