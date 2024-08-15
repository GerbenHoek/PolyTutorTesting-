module SymbolRoot.FactorInt 
   ( FactorInt
   , toFactorInt
   , eval
   , getPowers
   , commonPower) where 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

newtype FactorInt = FI [IntMap Int] 

getFactors :: FactorInt -> [IntMap Int]
getFactors (FI a) = a

factor :: Int -> [Int]
factor k = 
   if k == 1 || k == -1 then [k]
   else let (p, k') = getLeast k in 
            p:factor k' 
   where 
      getLeast m = 
         case [(n, m `div` n) | n <- [2..(abs m)], rem m n == 0]
         of []  -> (1, m)
            a:_ -> a

toFactorInt :: Int -> FactorInt
toFactorInt k = FI $ [IntMap.fromListWith (+) $ zip (factor k) (repeat 1)]

eval :: FactorInt -> Int
eval n = sum [product [a ^ b | (a, b) <- IntMap.toList k] | k <- getFactors n]

instance Eq FactorInt where 
   a == b = (eval a) == (eval b)

instance Ord FactorInt where 
   a `compare` b = (eval a) `compare` (eval b)

instance Show FactorInt where 
   show = show . eval  

adjustSign :: IntMap Int -> IntMap Int
adjustSign a = 
  if even $ IntMap.findWithDefault 0 (-1) a 
  then IntMap.insert 1 1 (IntMap.delete (-1) a)
  else IntMap.insert (-1) 1 (IntMap.delete 1 a)

instance Num FactorInt where 
   a + b = FI $ (getFactors a) ++ (getFactors b) 
   a * b = FI $ adjustSign <$> 
      ((IntMap.unionWith (+)) <$> (getFactors a) <*> (getFactors b))
   signum a = case a of FI [n] -> toFactorInt $ IntMap.findWithDefault (-1) 1 n
                        _      -> toFactorInt $ signum $ eval a 
   abs a = case a of FI [n] -> FI [IntMap.insert 1 1 (IntMap.delete (-1) n)]
                     _      -> toFactorInt $ abs $ eval a 
   negate a = case a of 
      FI [n] -> if IntMap.member 1 n 
                then FI [IntMap.insert 1 1 (IntMap.delete (-1) n)]
                else FI [IntMap.insert (-1) 1 (IntMap.delete 1 n)]                         
      _      -> toFactorInt $ negate $ eval a
   fromInteger = toFactorInt . fromInteger

clean :: FactorInt -> FactorInt
clean n = FI $ map f (getFactors n)
   where 
      f a = let c = IntMap.filter (/=0) a 
            in if IntMap.null c 
               then IntMap.singleton 1 1  
               else c

getPowers :: Int -> FactorInt -> (Int, FactorInt)
getPowers k a = 
   case a of 
      FI [n] -> (eval $ FI [IntMap.filter (/=0) $ IntMap.map (`div` k) n]
                ,  clean $ FI [IntMap.map (`rem` k) n])             
      _      -> getPowers k $ toFactorInt $ eval a

commonPower :: Int -> FactorInt -> (FactorInt, Int)
commonPower k a = case (abs a) of 
   FI [n] -> let p = gcdl $ k:(IntMap.elems $ IntMap.delete 1 n) 
             in (clean $ signum a * FI [IntMap.map (`div` p) n], k `div` p)      
   _      -> commonPower k $ toFactorInt $ eval a
   where gcdl as = foldr1 gcd as