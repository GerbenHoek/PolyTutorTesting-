module Polynomial.NumDiv where 
import Polynomial.SinglePoly
import SymbolRoot.SymbolRoot

pDiv
  :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> SinglePoly x a
     -> (SinglePoly x a, SinglePoly x a)
pDiv a b = pdiv a (abs b) 0 
   where 
      pdiv _ 0 _ = (0,0)
      pdiv 0 _ d = (d,0)
      pdiv p q d =
         let lp = leadCoeff p
             lq = leadCoeff q
             n = degree p 
             k = degree q
             x = getVar p 
             quo = constant lp * mvar x ^ (n - k)
             r = constant lq * deleteLead p - quo * deleteLead q 
         in if n >= k 
            then pdiv r q (quo + constant lq * d) 
            else (d, p)

quotient 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
quotient a b = fst $ pDiv a b

remainder 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
remainder a b = snd $ pDiv a b

isDivBy 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> Bool
isDivBy a b = remainder a b == 0 

isDivByAll 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> [SinglePoly x a]
      -> Bool
isDivByAll a = 
   all (a `isDivBy`) 

divideOut 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
divideOut a b = 
   let (q, r) = pDiv a b in
   if r/=0 then a else divideOut q b

divideOutAll 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> [SinglePoly x a]
      -> SinglePoly x a
divideOutAll a bs = 
   foldr (flip divideOut) a bs

euclid
  :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> SinglePoly x a
     -> [(SinglePoly x a)]
euclid a b = a:b:chain a b  
   where 
      chain r0 r1 = 
         if r1 /= 0  
         then r2:chain r1 r2
         else []
         where 
            r2 = remainder r0 r1

gcdp 
   :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> SinglePoly x a
     -> (SinglePoly x a)
gcdp a b = last $ init $ euclid a b

difference 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
difference a b = a `quotient` (gcdp a b) 

sturm
  :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> [(SinglePoly x a)]
sturm a = a:da:chain a da  
   where 
      da = derivative a 
      chain r0 r1 = 
         if degree r1 > 0  
         then r2:chain r1 r2
         else []
         where 
            r2 = negate $ remainder r0 r1

nrZero 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a -> Int
nrZero a = change (map signLow sturmChain) - change (map signHigh sturmChain)
      where sturmChain = sturm a
            signLow a = (-1) ^ (degree a) * (signum a)
            signHigh a = signum a
            change []       = 0
            change [x]      = 0 
            change (x:y:xs) = if signum x /= signum y 
                              then change (y:xs) + 1
                              else change (y:xs)

test 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> [SinglePoly x a]
      -> Maybe (Ordering)
test a bs 
   | t1 && t2 = Just EQ
   | t1       = Just GT
   | t2       = Just LT
   | otherwise = Nothing 
   where t1 = isDivByAll a bs 
         t2 = nrZero (divideOutAll a bs) == 0 

type RealPoly = SinglePoly String (SymbolRoot Integer)

pHighRoot :: Int -> Int -> RealPoly
pHighRoot k = constant . highRoot k