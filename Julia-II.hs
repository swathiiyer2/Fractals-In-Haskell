{-# LANGUAGE BangPatterns #-}

module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Colour
  
main :: IO ()
main = do 
     (progname, _) <- getArgsAndInitialize
     initialDisplayMode $= [DoubleBuffered]
     createWindow "Julia Set"
     displayCallback $= display
     mainLoop

display :: DisplayCallback
display = do
     clear[ColorBuffer]
     loadIdentity
     preservingMatrix drawJulia
     swapBuffers

drawJulia = renderPrimitive Points $ do
     mapM_ drawColoredPoint allPoints
         where
            drawColoredPoint (x,y) = do
            let color3f r g b = color $ Color3 r g (b::GLfloat) 
            if iterCount < maxIters `div` 3
                then do color3f 0 0 0 
            else if iterCount < 2* maxIters `div` 3
                then do color3f 0 0 1
            else do color3f 1 0 0 
            vertex $ Vertex2 x y 
            where iterCount = julia (0,0.7) (x,y) 0
 
width = 500 :: GLfloat
height = 500 :: GLfloat

--Set of all points in the plane
allPoints :: [(GLfloat, GLfloat)]
allPoints = [(x/width, y/height) | x <- [-width..width], y <- [-height..height]] 

--Gives next point in a Julia sequence
next :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
next (u, v) (x, y) = (x^2 - y^2 + u, 2*x*y + v)

--Returns length of sequence before diverging and max iterations have occurred
julia:: (GLfloat, GLfloat) ->(GLfloat, GLfloat) -> Int -> Int
julia !p !value !count
        |count > maxIters = maxIters
        |closeToOrigin value = julia p (next p value) (count+1)
        |otherwise = count

--Checks if point is close to origin
closeToOrigin :: (GLfloat, GLfloat) -> Bool
closeToOrigin (x,y) = (sqrt $ x^2 + y^2) < 2

--The size of the sequence to check for divergence
maxIters :: Int
maxIters = 100
     