{-# LANGUAGE FlexibleContexts #-}
module Files where

import Control.Lens hiding (element, children)
import Control.Monad.State
import Models

import System.FilePath

relativizeImg :: (MonadState RequestState m) =>
                  ImageMetadata MetadataImage -> m (ImageMetadata MetadataImage)
relativizeImg i@(ImageMetadata (MetadataUrl _) _) = return i
relativizeImg (ImageMetadata (MetadataFilePath iPath) iDesc) = do
  relPath <- relativizeLink iPath
  return $ ImageMetadata (MetadataFilePath relPath) iDesc

relativizeLink :: (MonadState RequestState m) => String -> m String
relativizeLink rel = gets $ (</> rel) . dropFileName . view requestFilePath
