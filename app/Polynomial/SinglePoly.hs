module Polynomial.SinglePoly where 
import Data.Maybe (isNothing, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)

type Term x a = (Maybe (x, Int), a)

showTerm ::
   (Num a, Eq a, Show a, Show x) => Term x a -> String
showTerm (mv, a) = 
   case mv of 
      Nothing     -> brack $ show a 
      Just (x, n) | a == 1 -> brack (show x) ++ "^" ++ show n 
                  | otherwise -> brack (show a) ++ "*" ++ brack (show x) ++ "^" ++ show n 
   where brack s = "(" ++ s ++ ")"

power :: Term x a -> Int 
power (mv, _) = case mv of 
   Nothing     -> 0
   Just (_, n) -> n

newtype SinglePoly x a = SP {getP :: Map (Maybe (x, Int)) a} 
   deriving (Eq, Ord)

instance Functor (SinglePoly x) where 
   fmap g = SP . Map.map g . getP 

toPoly :: (Eq a, Num a, Ord x) => [Term x a] -> SinglePoly x a
toPoly = cleanPoly . SP . Map.fromListWith (+)

getTerms :: SinglePoly x a -> [Term x a]
getTerms = Map.toList . getP 

instance (Num a, Eq a, Show a, Ord x, Show x) => Show (SinglePoly x a) where 
   show = intercalate " + " . map showTerm . getTerms 

cleanPoly
   :: (Num a, Ord x, Eq a) => SinglePoly x a -> SinglePoly x a
cleanPoly p = 
   let p' = Map.filter (/=0) $ Map.mapKeysWith (+) f $ getP p in 
   if null p' then SP $ Map.singleton Nothing 0 else SP p'
   where f (Just (_,0)) = Nothing
         f t = t 

constant :: (Eq a, Num a, Ord x) => a -> SinglePoly x a
constant a = toPoly [(Nothing, a)]

var :: (Eq a, Num a, Ord x) => x -> SinglePoly x a
var n = toPoly [(Just (n, 1), 1)]

mvar :: (Eq a, Num a, Ord x) => Maybe x -> SinglePoly x a
mvar mx = case mx of 
   Nothing -> toPoly [(Nothing, 1)]
   Just x  -> toPoly [(Just (x,1),1)] 

getVar :: SinglePoly x a -> Maybe x
getVar p = if null (getP p) 
   then Nothing 
   else fst <$> (fst $ Map.findMax (getP p))

mul :: (Num a, Eq x) => Term x a -> Term x a -> Term x a 
mul (ns, a) (ms, b) = (ks, a * b)
   where 
      ks 
         | isNothing ns && isNothing ms = Nothing
         | isNothing ns = ms
         | isNothing ms = ns 
         | otherwise    = let (x, n) = fromJust ns 
                              (y, m) = fromJust ms in 
                          if x == y  
                          then Just (x, n + m)
                          else error "exeption: unmatching variables" 

leadTerm :: (Ord x, Num a, Eq a) => SinglePoly x a -> Term x a
leadTerm = Map.findMax . getP . cleanPoly

leadCoeff :: (Ord x, Num a, Eq a) => SinglePoly x a -> a
leadCoeff = snd . leadTerm

deleteLead :: (Ord x, Num a, Eq a) => SinglePoly x a -> SinglePoly x a 
deleteLead = cleanPoly . SP . Map.deleteMax . getP . cleanPoly

constantVal :: (Ord x, Num a) => SinglePoly x a -> a
constantVal p = case Map.lookup Nothing (getP p) of 
   Nothing -> 0 
   Just a  -> a 

degree :: (Ord x, Num a, Eq a) => SinglePoly x a -> Int
degree = power . leadTerm 

monic :: (Fractional a, Ord x, Eq a) => SinglePoly x a  -> SinglePoly x a
monic p = fmap (*(1/an)) p
   where an = leadCoeff p

instance (Eq a, Num a, Ord x) => Num (SinglePoly x a) where 
   p + q = cleanPoly $ SP $ Map.unionWith (+) (getP p) (getP q)
   p * q = cleanPoly $ SP $ Map.fromListWith (+) $ 
                            mul <$> (getTerms p) <*> (getTerms q) 
   negate = fmap negate 
   abs p  = fmap  (* (signum $ leadCoeff p)) p
   signum = constant . signum . leadCoeff
   fromInteger = constant . fromInteger

derivative :: (Eq a, Num a, Ord x) => SinglePoly x a -> SinglePoly x a 
derivative = toPoly . map deriveTerm . getTerms
   where
      deriveTerm (p, a) = case p of 
         Nothing -> (Nothing, 0)
         Just (x, k)  -> (Just (x, k-1), fromIntegral k * a)