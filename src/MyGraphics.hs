module MyGraphics (initializeWorld, callShowWorld) where
import           Data.Array.IArray
import           Data.Fixed        (mod')
import           Data.IORef
import           Graphics.UI.GLUT
import qualified Parameters        as P
import           Fitness
import Types

callShowWorld :: IO ()
callShowWorld = mainLoopEvent >> postRedisplay Nothing

initializeWorld :: IORef World -> IO ()
initializeWorld worldRef = do
    _ <- getArgsAndInitialize
    let pixelsPerUnit = 10
        w = pixelsPerUnit * fromIntegral P.width
        h = pixelsPerUnit * fromIntegral P.height
    initialDisplayMode $= [RGBMode, DoubleBuffered]
    initialWindowSize $= Size w h
    (Size screenSizeX screenSizeY) <- get screenSize
    let initialPos = Position
            (fromIntegral (screenSizeX - w) `div` 2)
            (fromIntegral (screenSizeY - h) `div` 2)
    initialWindowPosition $= initialPos
    _ <- createWindow "Evolverbetert v1"
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fromIntegral w / fromIntegral pixelsPerUnit)
            0 (fromIntegral h / fromIntegral pixelsPerUnit)

    displayCallback $= showWorld worldRef
    actionOnWindowClose $= MainLoopReturns

showWorld :: IORef World -> IO ()
showWorld worldRef = do
    clear [ColorBuffer]
    world <- readIORef worldRef
    drawSquares world
    swapBuffers

drawSquares :: World -> IO ()
drawSquares world = renderPrimitive Quads $ mapM_ drawQuad P.worldCoods
    where
        ags = agents world
        drawQuad :: (Int,Int) -> IO ()
        drawQuad (x, y) = do
            currentColor $= c
            vertex $ Vertex2 x0 y0
            vertex $ Vertex2 x1 y0
            vertex $ Vertex2 x1 y1
            vertex $ Vertex2 x0 y1
            where
                x0 :: GLint
                x0 = fromIntegral x
                x1 = fromIntegral x + 1
                y0 = fromIntegral y
                y1 = fromIntegral y + 1
                c = colorHammDist (ags!(x,y)) (env world)

colorHammDist :: Agent -> Env -> Color4 Float
colorHammDist NoAgent _ = Color4 0 0 0 0
colorHammDist ag e = myHSV (1 - relHammDist) 0.5 1
    where relHammDist = fromIntegral (hammDist e ag) / fromIntegral P.nrGeneTypes

_colorFit :: Agent -> Env -> Color4 Float
_colorFit NoAgent _ = Color4 0 0 0 0
_colorFit ag e = myHSV (realToFrac fit) 0.7 1
        where fit = fitness e ag

-- | Colors from red to green, purple if fitness is optimal
myHSV :: Float -> Float -> Float -> Color4 Float
myHSV h s v
    | h' < 1    = plusM (x,0,c)
    | h' < 2    = plusM (c,0,x)
    | h' < 3    = plusM (c,x,0)
    | h' < 4    = plusM (x,c,0)
    | otherwise = plusM (0,x,c)
    where
        plusM (i, j, k) = Color4 (i+m) (j+m) (k+m) 1
        h' = h * 4
        x  = c * (1- abs (h' `mod'` 2 - 1))
        c = s * v
        m = v - c

-- hsv :: Int -> Float -> Float -> Color4 Float
-- hsv h s v
--     | h' < 1    = plusM (c,x,0) --Color4 (c+m) (x+m) m 1
--     | h' < 2    = plusM (x,c,0) --Color4 (x+m) (c+m) m 1
--     | h' < 3    = plusM (0,c,x) --Color4 m (c+m) (x+m) 1
--     | h' < 4    = plusM (0,x,c) --Color4 m (x+m) (c+m) 1
--     | h' < 5    = plusM (x,0,c) --Color4 (x+m) m (c+m) 1
--     | otherwise = plusM (c,0,x) --Color4 (c+m) m (x+m) 1
--     where
--         plusM (a, b, c) = Color4 (a+m) (b+m) (c+m) 1
--         h' = fromIntegral h / 60
--         c  = s * v
--         x  = c * (1- abs (h' `mod'` 2 - 1))
--         m = v - c

-- hSVtoRGB :: Int -> Double -> Double -> Color4 GLfloat
-- hSVtoRGB h s v = case h' of
--     0         -> Color4 c x 0 1
--     1         -> Color4 x c 0 1
--     2         -> Color4 0 c x 1
--     3         -> Color4 0 x c 1
--     4         -> Color4 x 0 c 1
--     5         -> Color4 c 0 x 1
--     where
--         h' = h `div` 60
--         c  = fromInteger $ floor $ s * v
--         x  = c * fromIntegral (1 - abs ((h' `mod` 2) - 1))
