{-# OPTIONS_GHC -Wno-type-defaults #-}

import Hurtle.Parser
import Hurtle.Types
import Text.Megaparsec ( parse )
import Graphics.Gloss (black, simulate, makeColor, Display(InWindow), Color, Picture)
import Hatch as H
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Fixed (mod')
import System.Random ( mkStdGen, Random(randomRs) )
import System.Environment (getArgs)
import Text.Megaparsec.Error (errorBundlePretty)

-- SETTING UP THE SIMULATION

-- get the recipe from the hogo file and then simulate on it
-- run this by running stack run <path to hogo file>
main :: IO ()
main = do
    recipe <- getRecipe
    -- print recipe
    simulator recipe


-- parse the hogoFile 
-- readFile path has to be fmapped because its in IO!!
-- getArgs gets a list of inputs, so head fpath is the first one! 
-- then handle errors by returning a pretty error, and returning an empty list
-- otherwise we turn the HogoProgram into a HurtleRecipe and return it
getRecipe :: IO HurtleRecipe
getRecipe = do
    let getHogo path = parse parseHogo "" <$> readFile path
    fpath <- getArgs
    hurtleThing <- getHogo $ head fpath
    case hurtleThing of
        Left err -> do
            putStrLn $ errorBundlePretty err 
            return []
        Right prog -> do
            let recipe = runRecipe [hurtleStart] prog
            return recipe


-- this is for debugging, set to a specific test
-- test :: String
-- test = "working3.hogo"
-- fpath :: String
-- fpath = "/Users/marcu/hogo/hurtle/examples/passing/" ++ test

-- Pt constructor 
data Pt = Pt {
    px :: Float,
    py :: Float
    } deriving (Show)

windowSize :: Pt
windowSize = Pt 800 800

window :: Display
window = InWindow "Slimy Hogo" (pt2pair windowSize) (0, 0)
    where pt2pair (Pt x y) = (round x, round y)



-- set up the simulation
simulator :: HurtleRecipe -> IO()
simulator recipe = simulate 
    window black 12                         -- window setup
    (recipe2slime recipe)                   -- initial state
    (renderSlimes (hurtleDraw recipe))      -- rendering function
    (slimesUpdate recipe)                   -- updating function



-- THE HURTLE STUFF

-- Hurtle data type
data Hurtle = Hurtle {
    x :: Float,     -- x coord
    y :: Float,     -- y coord
    angle :: Float, -- angle in radians bcos thats what gloss uses
    pen :: Bool     -- pen up/down
} deriving (Show)

--type synonym for clarity
type HurtleRecipe = [Hurtle]

-- initial hurtle starts at origin with pen down
hurtleStart :: Hurtle
hurtleStart = Hurtle 0 0 0 True

-- translate the hurtle's position by s units
-- we invert sin and cos from usual because hurtle uses like the bearing notation with 0 at the top
move :: Hurtle -> Float -> Hurtle
move h s = h {x = hx + s * sin hAngle, y = hy + s * cos hAngle}
    where
        hAngle = angle h
        hx = x h
        hy = y h

-- rotate the hurtle's angle by d degrees
-- its stored in radians because that's what gloss uses for rotation, so convert
turn :: Hurtle -> Float -> Hurtle
turn h d = h {angle = angle h + d * pi / 180}

-- fold across hogo program
runRecipe :: HurtleRecipe -> HogoProgram -> HurtleRecipe
runRecipe = foldl runCode

-- patternmatch on all the possible commands and do the apporpriate thing
-- i assume that the pen stays the same when you go home
runCode :: HurtleRecipe -> HogoCode -> HurtleRecipe
runCode l (GoForward s)  =  move  (head l) s            : l
runCode l (GoBackward s) =  move  (head l) (-s)         : l
runCode l (TurnLeft d)   =  turn  (head l) (-d)         : l
runCode l (TurnRight d)  =  turn  (head l) d            : l
runCode l GoHome  = hurtleStart {pen = pen $ head l}    : l
runCode l PenUp   = (head l)    {pen = False}           : l
runCode l PenDown = (head l)    {pen = True}            : l
runCode _ ClearScreen = [hurtleStart]
runCode l (Repeat n hp) = runRecipe [head l] (repeat' hp) ++ tail l
    where repeat' = concat . replicate n


-- start with doubles on the recipe, to get the adjacent pairs
-- uncurry hurtline to take in pairs
-- map across then superimposeall
-- as this all happens to just strail can just be composition of functions
hurtleDraw :: HurtleRecipe -> Image
hurtleDraw = superimposeAll . map (uncurry hurtline) . doubles

-- draw a line between two hurtles
-- if pens up dont do anything
hurtline :: Hurtle -> Hurtle -> Image
hurtline h1 h2
    | pen h1 = H.line c x1 y1 x2 y2
    | otherwise = H.blank
    where
        (x1, y1) = (x h1, y h1)
        (x2, y2) = (x h2, y h2)
        c = pts2color (Pt y2 x2) (Pt y1 x1)


doubles :: [a] -> [(a,a)]
doubles [] = []
doubles [_] = []
doubles (a:b:t) = (a,b) : doubles (b:t)


-- RENDERING STUFF

-- useful function that just converts from image to picture
image2pic :: Image -> Picture
image2pic = render . layout


-- first render the hogo render
-- then render the slimes atop
renderSlimes :: Image -> [Slime] -> Picture
renderSlimes recipe slime = image2pic $ recipe 
    <@> superimposeAll (map slime2pic slime)

-- offset the slime by the right amount
-- then draw slimetrail
slime2pic :: Slime -> Image
slime2pic slime =  offset (sx slime) (sy slime) goop
    <@> linePath (strail slime)

-- lines between each point in the trail
-- get all adjacent pairs with doubles
-- map strailine
-- then superimposeall to get all the lines
linePath :: [Pt] -> Image
linePath = superimposeAll . map strailine . doubles

-- take in two points and render a line between them with a color based on their positions
strailine :: (Pt, Pt) -> Image
strailine (p1, p2) = H.line c x1 y1 x2 y2
    where
        Pt x1 y1 = p1
        Pt x2 y2 = p2
        c = pts2color p1 p2

-- color function based on the points
-- its basically just a heuristic theres no reason to the number choices but it looks good
pts2color :: Pt -> Pt -> Color
pts2color (Pt x1 y1) (Pt x2 y2) = makeColor r g b 1
    where
        colorClamp = min 0.9 . max 0.2 . abs
        r = colorClamp $ x1 / 200
        g = colorClamp $ y1 / 200
        b = colorClamp $ (x2+y2) / 300


-- SLIME STUFF
{-
this paper is the inspiration for the model: https://uwe-repository.worktribe.com/output/980579
i took some creative liberties for artistic effect :D
i essentially create a bunch of individual slimes at each point along the hurtle, and then i run a slime simulation
i use the trails of all the slimes as well as the initial hurtle "trails" to determine behaviour 
instead of a trail map, i use distance to the closest point in the trail
-}

--Slime data type
data Slime = Slime {
    sx :: Float,        -- x coord of the slime
    sy :: Float,        -- y coord of the slime
    sangle :: Float,    -- angle of the slime
    strail :: [Pt],     -- list of points, the trail of the slime
    sseed :: Int        -- for generating random seeds
} deriving (Show)

--type synonym for clarity
type Slimes = [Slime]

-- maps the slimyUpdate function across all the slimesUpdate
-- i define this as a partially applyable function with the recipe,
-- so that i can take in the hogodrawing as input for the slimes to interact with :)
slimesUpdate :: HurtleRecipe -> ViewPort -> Float -> Slimes -> Slimes
slimesUpdate recipe _ _ slimes = map (slimyUpdate sensoryInput) slimes
    where sensoryInput = recipe2slime recipe ++ slimes

-- the update function for the slimes
-- takes in first all the slimes and then a specific slime to update
-- the update function composes 3 functions together
-- slimyMove and slimySense are inspired by move and sense stages from the model description in the paper
-- slimyTrail keeps the trail at a fixed length to not destroy computers, and adds the slimes position at each step
slimyUpdate :: Slimes -> Slime -> Slime
slimyUpdate ss s = (slimyTrail . slimySense ss . slimyMove distance) s
    where
        distance = 5
        viewDist = 25
        turnAmount = 0.2

        -- move slime forward by dist
        slimyMove :: Float -> Slime -> Slime
        slimyMove dist slime = slime {
            sx = sx slime + dist*cos (sangle slime),
            sy = sy slime + dist*sin (sangle slime)
        }     

        -- follows the model in paper
        -- i do the checking for walls here instead of in move though
        -- first i check if it can go forward, and turn around if it can't
        -- then i check the sensors 
        -- if the forward sensor is the greatest, turn randomly
        -- if left sensor biggest, turn right
        -- if right sensor biggest, turn left
        -- otherwise stay straight...
        slimySense :: Slimes -> Slime -> Slime
        slimySense slimes slime
            | abs (sx slime) > px windowSize/2 = slime {sangle = - sangle slime + pi}
            | abs (sy slime) > py windowSize/2 = slime {sangle = - sangle slime}
            | f > fR && f > fL = slime {sangle = sangle slime + randomRs (-turnAmount,turnAmount) (mkStdGen (sseed slime)) !! 1, sseed = sseed slime + 1}
            | fL < fR    = slime {sangle = sangle slime + turnAmount}
            | fR < fL    = slime {sangle = sangle slime - turnAmount}
            | otherwise = slime
            where
                -- all of the trail points across ALL slimes
                slimePts = concatMap strail slimes

                -- these model the sensors described in the papers
                -- i move forward the slime by the viewdistance and then gets its position as a point
                slimeF = let s' = slimyMove viewDist s in
                    Pt (sx s') (sy s')
                slimeFL =  let s' = slimyMove viewDist s {sangle = sangle s - turnAmount} in
                    Pt (sx s') (sy s')
                slimeFR  =  let s' = slimyMove viewDist s {sangle = sangle s + turnAmount} in
                    Pt (sx s') (sy s')
                -- as i dont have a trail map i model it instead as distance to the nearest line!
                f = min' $ map (distTo slimeF) slimePts
                fL = min' $ map (distTo slimeFL) slimePts
                fR = min' $ map (distTo slimeFR) slimePts

                -- min' is the same as minimum but doesn't fail for an empty list
                -- i pattern match on the list, choosing which element to keep each step
                min' [] = 0
                min' [a] = a
                min' (a:b:xs)
                    | a<b       = min' (a:xs)
                    | otherwise = min' (b:xs)
        
        
        -- its 200 - numberofslimes because i if theres many slimes the trails add up 
        slimyTrail :: Slime -> Slime
        slimyTrail slime =  slime {
            strail = take (200 - numberOfSlimes) 
                (Pt (sx slime) (sy slime) : strail slime)
        }

-- distance between two points (squared because squareroot is costly)
-- for comparison it doesnt need to be the actual distance
distTo :: Pt -> Pt -> Float
distTo p1 p2 = (px p1 - px p2)^2 + (py p1 - py p2)^2

numberOfSlimes :: Int
numberOfSlimes = 50

-- convert the hurtle recipe into a list of slimes
-- hurtle2slime converts an individual point in the hogo to a slime
-- because of cases like the amongus where there are significantly more points in curves than lines,
    -- i use takeSlime to try and even it out across the whole shape
-- the reason that the initial point is in the trail is 
-- because i also use this function to add the drawing into the sensory input of the slimes
-- so the initial position is important there as the slimes view the trail not the slimes!
recipe2slime :: HurtleRecipe -> Slimes
recipe2slime recipe = takeSlime $ map hurtle2slime recipe 
    where 
        hurtle2slime h = Slime 
            (x h) (y h)                     -- the position is the same
            (angle h + pi/2)                -- the angle should be rotated by 90 degrees because im using normal angles where 0 is right
            [Pt (x h) (y h)]                -- we want the initial point in the trail
            (round $ x h * y h `mod'` 1000) -- the seed is generated based on the slime's position so they act differently
        takeSlime = takeEvery (length recipe `div` numberOfSlimes)
            -- divide the length of the list by slimes to get the right amount of slimes

-- take every nth element of a list
-- takeEvery 0 a        -> every 0th element is the same as the original list
-- takeEvery 0 a        -> vacuously take every nth element
-- takeEvery n (x:xs)   -> take the head, then skip n-1 elements and do a little recursion
takeEvery :: Int -> [a] -> [a]
takeEvery 0 a = a
takeEvery _ [] = []
takeEvery n (x:xs) = x: takeEvery n (drop (n-1) xs)
