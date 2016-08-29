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

drawJulia = 
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
        where iterCount = divergence (take maxIters (julia (x,y))) 
 
width = 500 :: GLfloat
height = 500 :: GLfloat

--Set of all points in the plane
allPoints :: [(GLfloat, GLfloat)]
allPoints = [(x/width, y/height) | x <- [-width..width], y <- [-height..height]] 

--Gives next point in a Julia sequence
next :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
next (u, v) (x, y) = (x^2 - y^2 + u, 2*x*y + v)

--Generates a Julia sequence given c = 0 + 0.7i
julia :: (GLfloat, GLfloat) -> [(GLfloat, GLfloat)]
julia p =  iterate (next (0,0.7)) p

--Checks if point is close to origin
closeToOrigin :: (GLfloat, GLfloat) -> Bool
closeToOrigin (x,y) = (sqrt $ x^2 + y^2) < 2

--Checks if point is in Julia set by checking if its sequence diverges 
inJuliaSet :: (GLfloat, GLfloat) -> Int -> Bool
inJuliaSet p n = all closeToOrigin (take n (julia p))

--Returns length of sequence before diverging and max iterations have occurred 
divergence :: [(GLfloat, GLfloat)] -> Int
divergence sq = length $ takeWhile closeToOrigin sq

--The size of the sequence to check for divergence
maxIters :: Int
maxIters = 100
     