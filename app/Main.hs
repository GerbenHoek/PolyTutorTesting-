module Main where
import Polynomial.NumDiv
import Polynomial.SinglePoly

tt1 :: RealPoly
tt1 = (var "x" - 7)^2 - 8

tt2 :: RealPoly
tt2 = tt1^2

tt3 :: RealPoly
tt3 = tt1 * ((var "x")^2 + 1)

tt4 :: RealPoly
tt4 = tt1 * (var "x" - 1)

rs :: [RealPoly]
rs = [var "x" - (7 + pHighRoot 2 8), var "x" - (7 - pHighRoot 2 8)]

main :: IO ()
main = print $ test tt4 rs