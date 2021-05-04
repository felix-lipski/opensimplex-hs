module Main where

import System.Random
import System.Random.Shuffle
import Data.Bits
import Control.Monad
import System.IO
import Data.Bits.Floating
-- import Data.IntCast
import Graphics.Rendering.Chart.Easy

main :: IO ()
main = do
    -- perm <- init_perm 1
    -- putStrLn $ show $ (.&.) 256 $ toInteger $ coerceToWord (0.7 :: Float)
    putStrLn $ show $ noise2d [0..255] 4 4
    -- putStrLn $ show $ noise2d (init_perm 1) 1.17 0
    -- putStrLn $ show $ noise2d (init_perm 1) 1.19 0
    -- putStrLn $ show $ noise2d (init_perm 1) 1.20 0
    -- putStrLn $ show $ noise2d (init_perm 1) 1.21 0
    -- putStrLn $ show $ noise2d (init_perm 1) 1.25 0
    -- putStrLn $ show $ noise2d (init_perm 1) 1.26 0
    -- putStrLn $ show $ noise2d (init_perm 1) 1.269 0
    -- putStrLn $ show $ noise2d (init_perm 1) 1.27 0
    -- putStrLn $ show $ extrapolate (init_perm 1) 0.1 0.1 0.1 0.1
    mapM_ putStrLn $ map show $ map (noise2d (init_perm 1) 0) [0.00,0.01..1.00]
    -- mapM_ putStrLn $ map show $ take 1 $ init_perm 1

gradients :: [Int]
gradients = [ 5, 2, 2, 5,-5, 2,-2, 5, 5,-2, 2,-5,-5,-2,-2,-5]
normConstant :: Int
normConstant = 47
stretchConstant :: Float
stretchConstant = -0.211324865405187
squishConstant :: Float
squishConstant = 0.366025403784439

init_perm :: Int -> [Int]
init_perm seed =
    let gen = mkStdGen seed
    in shuffle' [0..255] 256 gen

perm_ind :: [Int] -> Int -> Int -> Int
perm_ind perm x y = (.&.) 14 $ (!!) perm $ (.&.) 255 $ (+) y $ (!!) perm $ (.&.) 255 x

extrapolate :: [Int] -> Int -> Int -> Float -> Float -> Float
extrapolate perm xsb_var ysb_var dx dy =
    let ind = perm_ind perm xsb_var ysb_var
        g1 = gradients !! ind
        g2 = gradients !! (ind + 1)
    in (fromIntegral g1) * dx + (fromIntegral g2) * dy

str_offs :: Float -> Float -> Float
str_offs x y = (*) stretchConstant $ x + y

xs :: Float -> Float -> Float
xs x y = (+) x $ str_offs x y

ys :: Float -> Float -> Float
ys x y = (+) y $ str_offs x y

xsb :: Float -> Float -> Int
xsb x y = floor $ xs x y

ysb :: Float -> Float -> Int
ysb x y = floor $ ys x y

xins :: Float -> Float -> Float
xins x y = (xs x y) - (fromIntegral $ xsb x y)

yins :: Float -> Float -> Float
yins x y = (ys x y) - (fromIntegral $ ysb x y)

in_sum :: Float -> Float -> Float
in_sum x y = (xins x y) + (yins x y)

xsb2 :: Float -> Float -> Int
xsb2 x y = if (in_sum x y) <= 1 then (xsb x y) else (xsb x y) + 1

ysb2:: Float -> Float -> Int
ysb2 x y = if (in_sum x y) <= 1 then (ysb x y) else (xsb x y) + 1

sqsh_offs :: Float -> Float -> Float
sqsh_offs x y = (*) squishConstant $ fromIntegral $ (xsb x y) + (ysb x y)

dx0 :: Float -> Float -> Float
dx0 x y = (-) x $ (+) (fromIntegral $ xsb x y) $ sqsh_offs x y

dy0 :: Float -> Float -> Float
dy0 x y = (-) y $ (+) (fromIntegral $ ysb x y) $ sqsh_offs x y

dx02 :: Float -> Float -> Float
dx02 x y = if (in_sum x y) <= 1 then dx0 x y else (dx0 x y) - (1 + 2 * squishConstant)

dy02 :: Float -> Float -> Float
dy02 x y = if (in_sum x y) <= 1 then dy0 x y else (dy0 x y) - (1 + 2 * squishConstant)

dx1 :: Float -> Float -> Float
dx1 x y = (dx0 x y) - (1 + squishConstant)

dy1 :: Float -> Float -> Float
dy1 x y = (dy0 x y) - (0 + squishConstant)

dx2 :: Float -> Float -> Float
dx2 x y = (dx0 x y) - (0 + squishConstant)

dy2 :: Float -> Float -> Float
dy2 x y = (dy0 x y) - (1 + squishConstant)

attn1 :: Float -> Float -> Float
attn1 x y = 2 - ((dx1 x y) ** 2 + (dy1 x y) ** 2)

attn2 :: Float -> Float -> Float
attn2 x y = 2 - ((dx2 x y) ** 2 + (dy2 x y) ** 2)

value :: Float -> Float -> [Int] -> Float
value x y perm =
    let a = if (attn1 x y) > 0 then (*) ((attn1 x y) ** 4) $ extrapolate perm (1 + xsb x y) (0 + ysb x y) (dx1 x y) (dy1 x y) else 0
        b = if (attn2 x y) > 0 then (*) ((attn2 x y) ** 4) $ extrapolate perm (0 + xsb x y) (1 + ysb x y) (dx2 x y) (dy2 x y) else 0
    in a + b

zins :: Float -> Float -> Float
zins x y = if (in_sum x y) <= 1 then 1 - in_sum x y else 2 - in_sum x y


bigif :: Float -> Float -> (Int, Int, Float, Float)
bigif x y =
    if (in_sum x y) <= 1 then
        (if (zins x y) > (xins x y) || (zins x y) > (yins x y) then
            (if (xins x y) > (yins x y) then (( 1 + xsb x y),(-1 + ysb x y),(-1 + dx0 x y),( 1 + dy0 x y))
                                        else ((-1 + xsb x y),( 1 + ysb x y),( 1 + dx0 x y),(-1 + dy0 x y))  )
        else ((1 + xsb x y),(1 + ysb x y),(-1 + (dx0 x y) - 2*squishConstant),(-1 + (dy0 x y) - 2*squishConstant)) )
    else
        (if (zins x y) < (xins x y) || (zins x y) < (yins x y) then
            (if (xins x y) > (yins x y) then ((2 + xsb x y),(0 + ysb x y),(-2 + (dx0 x y) - 2*squishConstant),(-0 + (dy0 x y) - 2*squishConstant))
                                        else ((0 + xsb x y),(2 + ysb x y),(-0 + (dx0 x y) - 2*squishConstant),(-2 + (dy0 x y) - 2*squishConstant)) )
        else ((xsb x y),(ysb x y),(dx0 x y),(dy0 x y)) )

attn0 :: Float -> Float -> Float
attn0 x y = 2 - (dx02 x y)**2 - (dy02 x y)**2

attn_ext :: Float -> Float -> Float
attn_ext x y =
    let (xsv_ext, ysv_ext, dx_ext, dy_ext) = bigif x y
    in 2 - dx_ext**2 - dy_ext**2

noise2d :: [Int] -> Float -> Float -> Float
noise2d perm x y =
    let value_var = value x y perm
        (xsv_ext, ysv_ext, dx_ext, dy_ext) = bigif x y
        a = if (attn0 x y)    > 0 then ((attn0 x y)**4) * extrapolate perm (xsb2 x y) (ysb2 x y) (dx02 x y) (dy02 x y) else 0
        b = if (attn_ext x y) > 0 then ((attn0 x y)**4) * extrapolate perm xsv_ext ysv_ext dx_ext dy_ext else 0
    in (value_var + a + b) / fromIntegral normConstant


























--
