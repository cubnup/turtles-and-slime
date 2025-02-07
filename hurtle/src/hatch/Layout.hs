--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Layout where

--------------------------------------------------------------------------------

import Graphics.Gloss (Picture, pictures, translate, blank)

--------------------------------------------------------------------------------

-- Our Arrange type represents some arrangement of images
data Arrange a = Vert (Arrange a) (Arrange a)
               | Horz (Arrange a) (Arrange a)
               | Supp (Arrange a) (Arrange a)
               | Leaf a
    deriving (Show, Functor)

-- We define a type synonym just to make dealing with Images a little nicer
type Image = Arrange Picture

--------------------------------------------------------------------------------
-- Combinator functions

-- Horizontal composition
infixr 4 <|>
(<|>) :: Image -> Image -> Image
a <|> b = Horz a b

infixr 3 <->
-- Vertical composition
(<->) :: Image -> Image -> Image
a <-> b = Vert a b

infixr 2 <@>
-- Superimposition
(<@>) :: Image -> Image -> Image
a <@> b = Supp a b

superimposeAll :: [Image] -> Image
superimposeAll = foldr (<@>) (Leaf blank)

-- Assume images to be exactly 200x125 for simplicity
imgW :: Int
imgW = 200
imgH :: Int
imgH = 125

type Size = (Int, Int)
type Pos = (Int, Int)

width :: Image -> Int
width = fst . size

height :: Image -> Int
height = snd . size

left :: Pos -> Int
left = fst

top :: Pos -> Int
top = snd

-- | `size` @img calculates the size of @img
size :: Image -> Size
size (Leaf _) = (imgW, imgH)
size (Vert i j) = (max iW jW, iH+jH)
    where (iW,iH) = size i
          (jW,jH) = size j
size (Horz i j) = (iW+jW, max iH jH)
    where (iW,iH) = size i
          (jW,jH) = size j
size (Supp i j) = (max iW jW, max iH jH)
    where (iW,iH) = size i
          (jW,jH) = size j

-- Convert from Images to Gloss's picture format
layout :: Image -> [(Pos, Picture)]
layout (Vert a b) =
    let tb = map (\((x,y), i) -> ((x, y - height a), i)) (layout b)
    in (layout a) ++ tb
layout (Horz a b) =
    let tb = map (\((x,y), i) -> ((x + width a, y), i)) (layout b)
    in layout a ++ tb
layout (Supp a b) = layout a ++ layout b
layout (Leaf p  ) = [((0,0), p)]

-- | `render` @pps translates pictures in @pps by their absolute
-- position and combines them into one picture.
render :: [(Pos, Picture)] -> Picture
render = pictures . map (\(p,i) -> 
    translate (fromIntegral $ left p) (fromIntegral $ top p) i)

--------------------------------------------------------------------------------
