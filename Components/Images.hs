{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Components.Images (renderImage) where

import Config
import Models

import Data.Map (lookup)
import System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Base64.Lazy as B64

import Control.Exception

import Control.Monad.Trans
import Text.Blaze.Html5 as H hiding (main, style)
import Text.Blaze.Html5.Attributes

_buildImgSrc :: (MonadIO m) => ImageMetadata MetadataImage -> m (Either String String)
_buildImgSrc (ImageMetadata (MetadataUrl st) _) = return . Right $ st
_buildImgSrc (ImageMetadata (MetadataFilePath st) _) = do
  let cType = getImgContentType $ tail $ takeExtension st
  imgContents <- getBase64EncodedImage st
  let imgSrcString = go <$> cType <*> imgContents
  return $ BLU.toString <$> imgSrcString where
  go imgType imgContents' = mconcat @BL.ByteString ["data:", BLU.fromString $ imgType <> ";base64;, ", imgContents']

renderImage :: (MonadIO m) => ImageMetadata MetadataImage -> m Html
renderImage imageData@(ImageMetadata _ iDesc) = do
  imgSrc <- _buildImgSrc imageData
  return $ H.div ! class_ "card" $ do
    case imgSrc of
      Left err -> do
        H.span ! class_ "text-error" $ do
          b "Error:"
          H.span $ toMarkup err
      Right srcString -> do
          H.div ! class_ "card-image p-centered" $ do
            img ! src (stringValue srcString) ! style "max-height: 500px; object-fill: contain;"
          H.div ! class_ "card-header" $ do
            H.div ! class_ "card-subtitle text-grey" $ toMarkup iDesc

handleNonExistentFileException :: IOException -> String
handleNonExistentFileException = displayException

getBase64EncodedImage :: (MonadIO m) => String -> m (Either String BL.ByteString)
getBase64EncodedImage fpath = liftIO $ catch
        (Right . B64.encode . BL.fromStrict <$> B.readFile fpath)
        (return . Left . handleNonExistentFileException)

getImgContentType :: String -> Either String String
getImgContentType k = case Data.Map.lookup k allowedImageTypesMap of
  Just res -> Right res
  Nothing -> Left $ "No data for extension " <> k
