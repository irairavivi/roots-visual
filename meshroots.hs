-- meshroots.hs
-- Create a mesh of points and find the roots over that mesh for a complex-valued polynomial in two variables

module MeshRoots where

import Data.Complex
import Polynomial.Roots
import Graphics.UI.GLUT hiding (initState)

-- Create a mesh of complex values --------------------------------------------------------------------------------------------------------
-- (a,b) : top-left of the mesh square
-- (c,d) : bottom-right of the mesh square
-- x_incr, y_incr : increments for the x and y axis, respectively
-- Output is a list of complex doubles corresponding to the point a+bi 
mesh :: (Double, Double) -> (Double, Double) -> Double -> Double -> [Complex Double]
mesh (a,b) (c,d) x_incr y_incr = [x :+ y | x <- [a,a+x_incr..c], y <- [b,b+y_incr..d]]

-- Defines the mesh size, i.e. this is the size the picture will be
coeffs :: [Complex Double]
coeffs = mesh (-4,-4) (4,4) 0.1 0.1

-- Enter the function and compute the roots using Laguerre's Method -----------------------------------------------------------------------

-- Example 1: x^3 + y^3 = 0
-- Write the polynomial as: 1(y)^3 + 0(y)^2 + 0(y) + x^3 = 0
-- Enter the coeffs in opposite order (lowest order term ----> highest order term): [x^3 , 0 , 0, 1]
-- Plug in d for x: [d^3 , 0 , 0, 1]

-- The first argument is the tolerance: 1.0e-6
-- The second argument is the number of allowed iterations: 300
-- The final argument is a list of the coefficients of the polynomial: [d^3 , 0 , 0, 1]

findRoots :: Complex Double -> [Complex Double] 
findRoots d = roots 1.0e-6 300 [d^3, 0, 0, 1]

-- Example 2: sin(xy) approximated by 7th degree taylor polynomial
--findRoots :: Complex Double -> [Complex Double] 
--findRoots d = roots 1.0e-6 300 [0, d, 0, (-(d^3)/6), 0, ((d^5)/120), 0, (-(d^7)/5040)]

-- Example 3: the elliptic curve y^2 - x^3 - 12x - 1 = 0
--findRoots :: Complex Double -> [Complex Double] 
--findRoots d = roots 1.0e-6 300 [-1 - 12*d - d^3, 0, 1]


-- Create the list of tuples --------------------------------------------------------------------------------------------------------------
coeffRoots :: Complex Double -> [Complex Double] -> [(GLfloat,GLfloat,GLfloat,GLfloat)]
coeffRoots m r = [(getReal m, getImag m, getReal x, getImag x) | x <- r]

-- Compute all the roots over the mesh
allRoots :: [[Complex Double]]
allRoots = map findRoots coeffs

-- Add all those roots to a list in the form of 4-tuples with their correspoinding coefficient
allPoints :: [(GLfloat,GLfloat,GLfloat,GLfloat)]
allPoints = concat (zipWith coeffRoots coeffs allRoots)

-- Helpful functions ----------------------------------------------------------------------------------------------------------------------
-- Functions to get the real and imaginary parts of the coefficients and roots
getReal :: Complex Double -> GLfloat
getReal (a :+ b) = doubleToGLfloat a

getImag :: Complex Double -> GLfloat
getImag (a :+ b) = doubleToGLfloat b

-- Utility functions
realToReal :: (Real a, Fractional b) => a -> b
realToReal = fromRational . toRational

doubleToGLfloat :: Double -> GLfloat
doubleToGLfloat = realToReal

