{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Layout (
  generateAccordionPage, AppRender
  ) where

import Models
import Tree
import Logic
import Helpers
import Files
import Config

import Components.Accordion
import Components.Images
import Components.Grid

import System.FilePath

import Text.Blaze.Html5 as H hiding (main, style)
import Text.Blaze.Html5.Attributes

extractEither :: (Monad m) => Either (m a) (m b) -> m (Either a b)
extractEither e = case e of
  Left eA -> Left <$> eA
  Right eB -> Right <$> eB

_fmapLeft :: (a -> c) -> Either a b -> Either c b
_fmapLeft fn (Left x) = Left $ fn x
_fmapLeft _ (Right x) = Right x

_renderImageGrid :: [ImageMetadata MetadataImage] -> AppRender Html
_renderImageGrid ims = do
  imHtmls <- sequence $ renderImage <$> ims
  return $ renderGridGaps imHtmls

expandImageTree :: Tree OrgHeader OrgLine -> AppRender (Tree OrgHeader (ImageMetadata MetadataImage))
expandImageTree (Tree hd ls) = do
  newLines <- sequence $ extractEither <$>
    _fmapLeft (relativizeImg @AppRender . expandImageUrl) <$>
    (expandEitherList createImages $ go <$> ls)
  return $ Tree hd newLines
  where
  go (Left x) = Left x
  go (Right x) = Right $ expandImageTree x

renderImageTree :: Tree OrgHeader (ImageMetadata MetadataImage) -> AppRender Html
renderImageTree = renderTree _renderImageGrid

spectreStylesheet :: Html
spectreStylesheet = link ! rel "stylesheet" ! href "https://unpkg.com/spectre.css/dist/spectre.min.css"

userCssStylesheet :: Html
userCssStylesheet = link ! rel "stylesheet" ! href (stringValue (staticDirectory </> "user.css"))

pageSkeleton :: Html -> Html
pageSkeleton bodyHtml = docTypeHtml $ do
  H.head $ do
    H.title "Page" -- TODO Base this on the org file name
    spectreStylesheet
  body $ do bodyHtml

generateAccordionPage :: String -> Tree OrgHeader OrgLine -> IO Html
generateAccordionPage fileRoot tre = pageSkeleton <$> runApp (expandImageTree tre >>= renderImageTree) fileRoot
