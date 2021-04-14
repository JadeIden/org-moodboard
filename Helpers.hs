{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helpers where

import Control.Monad.State
import Control.Lens (view, over, Lens')
import Control.Exception

simpleCatch :: IO x -> IO (Either String x)
simpleCatch fn = catch (Right <$> fn) (return . Left . displayException @IOException)

modifyThenGet :: (MonadState a m) => (a -> a) -> m a
modifyThenGet inc = do
  x <- get
  let x' = inc x
  put x'
  return x'

modifyThenGet' :: (MonadState a m) => Lens' a x -> (x -> x) -> m x
modifyThenGet' lens modFn = do
  x <- modifyThenGet (over lens modFn)
  return $ view lens x

expandEitherList :: (a -> [c]) -> [Either a b] -> [Either c b]
expandEitherList _ [] = []
expandEitherList fn ((Right x):xs) = Right x : expandEitherList fn xs
expandEitherList fn ((Left x):xs) = (Left <$> fn x) ++ expandEitherList fn xs
