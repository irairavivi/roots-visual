# roots-visual

## Preliminaries
Haskell code to visualize the zero set of complex-valued polynomials in two variables. This is part of my mathematics senior thesis project at Wake Forest University.

Part of the graphics implementation is based on the 3d-graphics-examples package, available [here](https://hackage.haskell.org/package/3d-graphics-examples).

To use this code, you must have the following installed:

  1. GHC (the Haskell compiler), available [here](https://www.haskell.org/downloads)
  2. The base package, available [here](https://hackage.haskell.org/package/base)
  3. The dsp package, available [here](https://hackage.haskell.org/package/dsp)
  4. The GLUT package, available [here](https://hackage.haskell.org/package/GLUT)
  5. The random package, available [here](https://hackage.haskell.org/package/random)
  6. The OpenGL package, available [here](https://hackage.haskell.org/package/OpenGL)

## Using the code

### In meshroots.hs

In this file, you must define the size of the picture, that is, you define the dimensions of the mesh over which we compute the roots of the polynomials. To do so, you can edit the line

```haskell
-- Defines the mesh size, i.e. this is the size the picture will be
coeffs :: [Complex Double]
coeffs = mesh (-4,-4) (4,4) 0.1 0.1
```

with your desired dimensions. Here, the first ordered pair denotes the top-left point of the mesh, the second ordered pair denotes the bottom-right of the mesh, and the last two numbers are the increments for the x-axis and y-axis, respectively. 

This file is also where you input the function of which you wish to visualize the zero set. For example, to graph the zero set of the function x^3 + y^3 = 0, first, write out the polynomial as 1(y)^3 + 0(y)^2 + 0(y) + x^3 = 0. From this you can create the list of coefficients, entering them from the lowest order term to the highest order term, and replacing x with d. You can also change the tolerance level and number of allowed iterations in Laguerre's Method to find the roots. The code will look like

```haskell
-- Example 1: x^3 + y^3 = 0
-- Write the polynomial as: 1(y)^3 + 0(y)^2 + 0(y) + x^3 = 0
-- Enter the coeffs in opposite order (lowest order term ----> highest order term): [x^3 , 0 , 0, 1]
-- Plug in d for x: [d^3 , 0 , 0, 1]

-- The first argument is the tolerance: 1.0e-6
-- The second argument is the number of allowed iterations: 300
-- The final argument is a list of the coefficients of the polynomial: [d^3 , 0 , 0, 1]

findRoots :: Complex Double -> [Complex Double] 
findRoots d = roots 1.0e-6 300 [d^3, 0, 0, 1]
```

There are two other precoded examples in the code.

### In rootsgraph.hs

In this file, you must select the visualization method you wish to use. To use the first method, uncomment the following

```haskell
-- METHOD 1: Using magnitude sqrt(c^2 + d^2) as height, angle arctan(c, d) as color
create_tuple :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
create_tuple (a, b, c, d) = (a, b, mag c d)

-- Convert theta to a hue
theta_to_h :: (GLfloat,GLfloat) -> GLfloat
theta_to_h (a,b) = 360 * (theta / (2 * pi)) where theta = arctan (a,b)
```

and comment out the the other two methods. To use the second method, uncomment the following

```haskell
-- METHOD 2: Using angle arctan(c, d) as height, magnitude sqrt(c^2 + d^2) as color
create_tuple :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
create_tuple (a, b, c, d) = (a, b, arctan (c,d))

-- Convert theta to a hue
theta_to_h :: (GLfloat,GLfloat) -> GLfloat
theta_to_h (a,b) = 360 * (theta / (2 * pi)) where theta = mag a b
```

and comment out the other two methods. To use the third method, uncomment the following 

```haskell
-- METHOD 3: Using real part of y (c) as height, imaginary part of y (d) as color
create_tuple :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
create_tuple (a, b, c, d) = (a, b, c)

-- Convert theta to a hue
theta_to_h :: (GLfloat,GLfloat) -> GLfloat
theta_to_h (a,b) = 360 * (theta / (2 * pi)) where theta = b
```

and comment out the other two methods. You must recompile and rerun the code everytime you change the visualization method.

If you want to change the size of the rendered points, you can edit the line 

```haskell
pointSize $= 5 -- This is how big your points will show up
```

with your desired point size.

### Compiling and running
To compile the code, use the command 

```
ghc --make -package GLUT -o rootsgraph rootsgraph.hs 
```

To run the code, use the command 

```
./rootsgraph
``` 

The positive axes are solid lines while the negative axes are stippled lines, with red corresponding to the x-axis, pink corresponding to the y-axis, and yellow corresponding to the z-axis. You can use the arrow keys to view the image from different perspectives. Shift^X closes the window.
