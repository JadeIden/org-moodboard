{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Models (
        OrgLine (Line), OrgHeader (Header), OrgDocLine
        , ImageMetadata (ImageMetadata), OrgSection, AppRender (AppRender), RequestState (RequestState),
          MetadataRawString (MetadataRawString), MetadataImage (MetadataFilePath, MetadataUrl),
          accordionIdx, requestFilePath,
          imagePath, imageDescription, title, children, unFix, runApp', runApp
        , olLinks, olRestOfLine
) where

import Tree (Tree, Nested, isAbove, children, unFix)
import Control.Monad.State
import Control.Lens hiding (element, children)
import Control.Lens.TH ()

data RequestState = RequestState {
  _accordionIdx :: Int,
  _requestFilePath :: String
}

$(makeLenses ''RequestState)

newtype AppRender a = AppRender {
  runApp' :: StateT RequestState IO a
} deriving (Functor, Applicative, Monad, MonadState RequestState, MonadIO)

runApp :: AppRender a -> String -> IO a
runApp k st = fst <$> runStateT (runApp' k) (RequestState 1 st)

data OrgHeader = Header {
  depth :: Int,
  keyword :: Maybe String,
  priority :: Maybe Char,
  title :: String,
  tags :: Maybe [String]
} deriving (Show, Eq)

data OrgLine = Line {
  olLinks :: [String],
  olRestOfLine :: String
} deriving (Show, Eq)

type OrgDocLine = Either OrgHeader OrgLine

instance Nested OrgHeader where
  isAbove h1 h2 = depth h1 < depth h2

type OrgSection = Tree OrgHeader OrgLine

data ImageMetadata f = ImageMetadata {
  imagePath :: f,
  imageDescription :: String
} deriving Functor

newtype MetadataRawString = MetadataRawString String
data MetadataImage = MetadataFilePath String | MetadataUrl String
