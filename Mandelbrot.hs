module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Colour

width = 500 :: GLfloat
height = 500 :: GLfloat
  
main :: IO ()
main = do 
     (progname, _) <- getArgsAndInitialize
     initialDisplayMode $= [DoubleBuffered]
     createWindow "Mandelbrot Set"
     displayCallback $= display
     mainLoop

display :: DisplayCallback
display = do
     clear[ColorBuffer]
     loadIdentity
     preservingMatrix drawMandelbrot
     swapBuffers

drawMandelbrot = 
     renderPrimitive Points $ do
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
        where iterCount = divergence (take maxIters (mandelbrot (x,y)))
 
--Set of all points in the plane
allPoints :: [(GLfloat, GLfloat)]
allPoints = [(x/width, y/height) | x <- [-width..width], y <- [-height..height]] 

--Gives next point in a mandelbrot sequence
next :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
next (u, v) (x, y) = (x^2 - y^2 + u, 2*x*y + v)

--Generates a mandelbrot sequence given an initial point
mandelbrot :: (GLfloat, GLfloat) -> [(GLfloat, GLfloat)]
mandelbrot p =  iterate (next p) (0,0) 

--Checks if point is close to origin
closeToOrigin :: (GLfloat, GLfloat) -> Bool
closeToOrigin (x,y) = (sqrt $ x^2 + y^2) < 2

--Checks if point is in mandelbrot set by checking if its sequence diverges 
inMandelbrotSet :: (GLfloat, GLfloat) -> Int -> Bool
inMandelbrotSet p n = all closeToOrigin (take n (mandelbrot p))

--Returns length of sequence before diverging and max iterations have occurred 
divergence :: [(GLfloat, GLfloat)] -> Int
divergence sq = length $ takeWhile closeToOrigin sq

--The size of the sequence to check for divergence
maxIters :: Int
maxIters = 100
     