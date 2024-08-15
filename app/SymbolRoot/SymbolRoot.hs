module SymbolRoot.SymbolRoot where 
import SymbolRoot.FactorInt hiding (eval)
import qualified SymbolRoot.FactorInt as FI (eval)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

newtype SymbolRoot n = SR (Map (Int, FactorInt) n)
   deriving (Eq, Ord) 

getRoots :: SymbolRoot n -> Map (Int, FactorInt) n
getRoots (SR n) = n

eval :: Integral n => SymbolRoot n -> Double
eval r = sum [f a| a <- Map.toList $ getRoots r] 
   where 
      f ((p, k), n) = 
         (fromIntegral n) * (fromIntegral (FI.eval k)) ** (1 / (fromIntegral p)) 

sign :: (Integral n, Integral m) => SymbolRoot n -> m 
sign = signum . ceiling . eval 

type Root n = ((Int, FactorInt), n)

showRoot :: (Integral n, Show n) => Root n -> String 
showRoot ((p, k), a) 
   | a == 1 && k /= 1 && p /= 1 = show p ++ "^√(" ++ show k ++ ")" 
   | k == 1 = show a 
   | p == 1 = show $ p * FI.eval k
   | otherwise = show a ++ " * " ++ show p ++ "^√(" ++ show k ++ ")" 

instance (Integral n, Show n) => Show (SymbolRoot n) where 
   show = intercalate " + " . map showRoot . Map.toList . getRoots  


(.*.) :: Num n => Root n -> Root n -> Root n 
((p, n), a) .*. ((q, m), b) = ((p', k'), a * b * fromIntegral c)
   where (k', p') = commonPower (p*q) k 
         (c, k)   = getPowers (p*q) (n^q * m^p)

zeroCoef :: Integral n => SymbolRoot n -> SymbolRoot n
zeroCoef r = 
   let c = Map.filter (/=0) (getRoots r) in 
   if Map.null c then SR $ Map.singleton (1,1) 0 else SR c 

instance Integral n => Num (SymbolRoot n) where 
   a + b = zeroCoef $ SR $ Map.unionWith (+) (getRoots a) (getRoots b)
   a * b = zeroCoef $ SR $ Map.fromListWith (+) $
       (.*.) <$> (Map.toList $ getRoots a) <*> (Map.toList $ getRoots b)
   signum = fromIntegral . sign 
   fromInteger a = SR $ Map.singleton (1, 1) (fromInteger a)
   negate a = SR $ Map.map (* (-1)) (getRoots a)
   abs a = SR $ Map.map (* (sign a)) (getRoots a)

highRoot :: Integral n => Int -> Int -> SymbolRoot n 
highRoot k n = 1 * (SR $ Map.singleton (k, toFactorInt n) 1)