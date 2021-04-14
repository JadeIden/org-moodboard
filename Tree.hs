{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module Tree where

data FixTree h l r = FixTree {
  header :: h,
  children :: [Either l r] } deriving (Show, Functor, Eq)

newtype Fix f = Fix { unFix :: f (Fix f) }

newtype BFTree h l = BFTree (Tree h l)
newtype EndoTree l = EndoTree (Tree l l)

type Tree h l = Fix (FixTree h l)

{-# COMPLETE Tree #-}

pattern Tree :: forall h l.
                      h -> [Either l (Fix (FixTree h l))] -> Fix (FixTree h l)
pattern Tree h l = Fix (FixTree h l)

instance (Eq h, Eq l) => Eq (Tree h l) where
  (Tree hd1 lns1) == (Tree hd2 lns2) = (hd1 == hd2) && (lns1 == lns2)

instance (Show h, Show l) => Show (Tree h l) where
  show = show . unFix

_isUnder :: (Nested h) => h -> Either h l -> Bool
_isUnder h = either (isAbove h) (const True)

structure :: (Nested h) => [Either h l] -> h -> [Either l (Tree h l)]
structure [] _ = []
structure (l:ls) h = case l of
  Left hd -> let (tree, rest) = span (_isUnder hd) ls in
    Right (Fix (FixTree hd $ structure tree hd)) : structure rest h
  Right ln -> Left ln : structure ls h

_bimapTree :: (h -> h') -> (l -> l') -> Tree h l -> Tree h' l'
_bimapTree hf lf (Fix (FixTree h lns)) = Fix (FixTree (hf h) (go <$> lns)) where
  go (Left a) = Left $ lf a
  go (Right x) = Right $ _bimapTree hf lf x

class Nested a where
  isAbove :: a -> a -> Bool
