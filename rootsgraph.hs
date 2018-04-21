-- rootsgraph.hs
-- Plots the points from meshroots.hs using one of the three visualization methods
-- Compile: ghc --make -package GLUT -o rootsgraph rootsgraph.hs 
-- Run: ./rootsgraph

{-# LANGUAGE FlexibleContexts #-}

-- Import necessary packages --------------------------------------------------------------------------------------------------------------
import System.Random
import Data.List
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT hiding (initState)
import Data.IORef
import Data.Char (toUpper)
import Control.Monad
import MeshRoots

-- Set up initial values for camera panning and zooming -----------------------------------------------------------------------------------
data State = State {
  zoom       :: Double,           
  pan        :: (Double, Double), 
  viewPhi    :: Double,           
  viewTheta  :: Double        
}

initState = State {
  zoom      = 1,
  pan       = (0,0),
  viewPhi   = -80,
  viewTheta = 30
}

-- Main program to read in points from a file and open the graphic window -----------------------------------------------------------------
main = do
    (progName,_) <- getArgsAndInitialize
    createWindow "Visualization of Roots"
    windowSize $= Size 800 800
    depthFunc $= Just Less
    state <- newIORef initState
    displayCallback $= display state points
    keyboardMouseCallback $= Just (keyboard state)
    mainLoop

-- Orthogonal projection set up -----------------------------------------------------------------------------------------------------------
projection xl xu yl yu zl zu = do
  matrixMode $= Projection
  loadIdentity
  ortho xl xu yl yu zl zu
  matrixMode $= Modelview 0

-- Create the points from meshroots.hs -----------------------------------------------------------------------------------------------------
points :: [(GLfloat,GLfloat,GLfloat,GLfloat)]
points = allPoints

-- Select visualization method: -----------------------------------------------------------------------------------------------------------
-- Let a root be represented by (x, y) where x = a + bi and y = c + di
-- Then we can represent this root by the tuple (a, b, c, d)

mag :: GLfloat -> GLfloat -> GLfloat
mag c d = sqrt(c^2 + d^2)

-- METHOD 1: Using magnitude sqrt(c^2 + d^2) as height, angle arctan(c, d) as color
create_tuple :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
create_tuple (a, b, c, d) = (a, b, mag c d)

-- Convert theta to a hue
theta_to_h :: (GLfloat,GLfloat) -> GLfloat
theta_to_h (a,b) = 360 * (theta / (2 * pi)) where theta = arctan (a,b)

-- METHOD 2: Using angle arctan(c, d) as height, magnitude sqrt(c^2 + d^2) as color
--create_tuple :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
--create_tuple (a, b, c, d) = (a, b, arctan (c,d))

-- Convert theta to a hue
--theta_to_h :: (GLfloat,GLfloat) -> GLfloat
--theta_to_h (a,b) = 360 * (theta / (2 * pi)) where theta = mag a b

-- METHOD 3: Using real part of y (c) as height, imaginary part of y (d) as color
--create_tuple :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
--create_tuple (a, b, c, d) = (a, b, c)

-- Convert theta to a hue
--theta_to_h :: (GLfloat,GLfloat) -> GLfloat
--theta_to_h (a,b) = 360 * (theta / (2 * pi)) where theta = b


-- Prepare the points to be graphed -------------------------------------------------------------------------------------------------------
-- Creates the 3-tuple of the x, y, and z coordinates of the points 
myPoints :: [(GLfloat, GLfloat, GLfloat, GLfloat)] -> [(GLfloat,GLfloat,GLfloat)]
myPoints points = map create_tuple points-- This can be parallelized

create_color :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
create_color (a, b, c, d) = (hsl_to_r (c,d), hsl_to_g (c,d), hsl_to_b (c,d))

-- Creates the color of the points
myColors :: [(GLfloat, GLfloat, GLfloat, GLfloat)] -> [(GLfloat,GLfloat,GLfloat)]
myColors points = map create_color points

-- Convert HSL color to RGB color ---------------------------------------------------------------------------------------------------------
-- Compute theta, values between 0 to 2pi
arctan :: (GLfloat,GLfloat) -> GLfloat
arctan (a,b)
     | a > 0 && b >= 0 = atan(b/a)
     | a < 0 && b >= 0 = pi + atan(b/a)
     | a < 0 && b <= 0 = pi + atan(b/a)
     | a > 0 && b <= 0 = 2*pi + atan(b/a)
     | a == 0 && b >=0 = pi/2
     | a == 0 && b < 0 = (3 * pi)/2

-- Convert HSL value to RGB, hardcode S = 1.00 and L = 0.80 for pastel colors
get_tempR :: (GLfloat,GLfloat) -> GLfloat
get_tempR (a,b) 
     | tempR > 1 = tempR - 1
     | otherwise = tempR
     where
        tempR = ((theta_to_h (a,b)) / 360) + (1/3)

hsl_to_r :: (GLfloat,GLfloat) -> GLfloat
hsl_to_r (a,b) 
     | (6 * tempR) < 1 = temp2 + ((temp1 - temp2) * 6 * tempR)
     | (2 * tempR) < 1 = temp1
     | (3 * tempR) < 2 = temp2 +((temp1 - temp2) * ((2/3) - tempR) * 6)
     | otherwise = temp2
     where
        tempR = get_tempR (a,b)
        temp1 = 1.0
        temp2 = 0.6

hsl_to_g :: (GLfloat,GLfloat) -> GLfloat
hsl_to_g (a,b)
     | (6 * tempG) < 1 = temp2 + ((temp1 - temp2) * 6 * tempG)
     | (2 * tempG) < 1 = temp1
     | (3 * tempG) < 2 = temp2 +((temp1 - temp2) * ((2/3) - tempG) * 6)
     | otherwise = temp2
     where
        tempG = (theta_to_h (a,b)) / 360 -- already between 0 and 1
        temp1 = 1.0
        temp2 = 0.6

get_tempB :: (GLfloat,GLfloat) -> GLfloat
get_tempB (a,b) 
     | tempB < 0 = tempB + 1
     | otherwise = tempB
     where
        tempB = ((theta_to_h (a,b)) / 360) - (1/3)

hsl_to_b :: (GLfloat,GLfloat) -> GLfloat
hsl_to_b (a,b)
     | (6 * tempB) < 1 = temp2 + ((temp1 - temp2) * 6 * tempB)
     | (2 * tempB) < 1 = temp1
     | (3 * tempB) < 2 = temp2 +((temp1 - temp2) * ((2/3) - tempB) * 6)
     | otherwise = temp2
     where
        tempB = get_tempB (a,b)
        temp1 = 1.0
        temp2 = 0.6

-- Convert points and colors to graphics datatypes ----------------------------------------------------------------------------------------
graph_points :: (GLfloat,GLfloat,GLfloat) -> IO()
graph_points (x,y,z) = do
    color$Color3 (hsl_to_r (x,y)) (hsl_to_g (x,y)) ((hsl_to_b (x,y)) :: GLfloat)
    vertex$Vertex3 x y z

graph_point_and_color :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> IO()
graph_point_and_color (x,y,z) (r,g,b) = do
  color$Color3 r g b
  vertex$Vertex3 x y z 

-- Display function -----------------------------------------------------------------------------------------------------------------------
display state points = do
    curState <- get state
    clearColor $= Color4 1.0 1.0 1.0 1 -- Background color (white)
    clear [ColorBuffer,DepthBuffer]

    let z = realToReal $ zoom curState
    --projection (-z) (z) (-z) (z) (-100) (100)
    --projection (-5) (5) (-5) (5) (-5) (5)
    --projection (-2) (2) (-2) (2) (-2) (2) --changing the numbers here "zooms" in or out
    projection (-15) (15) (-15) (15) (-15) (15) -- This is how big your axes are
    loadIdentity

    let (xPan, yPan) = pan curState

    translate $ Vector3 (doubleToGLfloat xPan) (doubleToGLfloat yPan) 0

    rotate (doubleToGLfloat $ negate $ viewTheta curState) $ Vector3 1 1 0 
    rotate (doubleToGLfloat $ viewPhi curState) $ Vector3 1 0 0
    rotate (doubleToGLfloat $ viewTheta curState) $ Vector3 1 1 0

    pointSize $= 5 -- This is how big your points will show up
    renderPrimitive Points $ do
        zipWithM_ graph_point_and_color (myPoints points) (myColors points)      

    -- Positive axes are solid
    lineWidth $= 3
    lineStipple $= Nothing
    renderPrimitive Lines $ do
        -- x-axis
        color$Color3 0.675 0.0 (0.0 :: GLfloat) -- Red
        vertex$Vertex3 0 0 (0 :: GLfloat)
        vertex$Vertex3 10 0 (0 :: GLfloat)
        -- y-axis
        color$Color3 1 0.439 (0.533 :: GLfloat) -- Pink
        vertex$Vertex3 0 0 (0 :: GLfloat)
        vertex$Vertex3 0 10 (0 :: GLfloat)
        -- z-axis
        color$Color3 1 0.765 (0.0 :: GLfloat) -- Yellow
        vertex$Vertex3 0 0 (0 :: GLfloat)
        vertex$Vertex3 0 0 (10 :: GLfloat)

    -- Negative axes are stippled
    lineWidth $= 3
    lineStipple $= (Just (2, 0xAAAA))
    renderPrimitive Lines $ do
        -- x-axis
        color$Color3 0.675 0.0 (0.0 :: GLfloat) 
        vertex$Vertex3 (-10) 0 (0 :: GLfloat)
        vertex$Vertex3 0 0 (0 :: GLfloat)
        -- y-axis
        color$Color3 1 0.439 (0.533 :: GLfloat) 
        vertex$Vertex3 0 0 (0 :: GLfloat)
        vertex$Vertex3 0 (-10) (0 :: GLfloat)
        -- z-axis
        color$Color3 1 0.765 (0.0 :: GLfloat)
        vertex$Vertex3 0 0 (0 :: GLfloat)
        vertex$Vertex3 0 0 ((-10) :: GLfloat)
    flush

keyboard state (SpecialKey specialKey) Down mod _ = do
  curState <- get state
  let
    phi = viewPhi curState
    theta = viewTheta curState
    ctrlPressed = ctrl mod == Down
    z = zoom curState
    (x,y) = pan curState

  if ctrlPressed
    then case specialKey of
      KeyLeft  -> state $= curState { pan = (x - 0.05 * z, y) }
      KeyRight -> state $= curState { pan = (x + 0.05 * z, y) }
      KeyDown  -> state $= curState { pan = (x, y - 0.05 * z) }
      KeyUp    -> state $= curState { pan = (x, y + 0.05 * z) }
      _ -> return ()
    else case specialKey of
      KeyLeft  ->
        state $= curState { viewPhi = if phi<0 then phi+355 else phi-5 }
      KeyRight ->
        state $= curState { viewPhi = if phi>360 then phi-355 else phi+5 }
      KeyDown  ->
        state $= curState { viewTheta = if theta<=0 then 0 else theta-5 }
      KeyUp ->
        state $= curState { viewTheta = if theta>=90 then 90 else theta+5 }
      _ -> return ()

  postRedisplay Nothing

keyboard state (Char charKey) Down _ _ = do
  curState <- get state

  case charKey of
  	'X' ->do
  		leaveMainLoop

keyboard _ _ _ _ _ = return ()

