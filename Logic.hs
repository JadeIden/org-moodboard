{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Logic (
  OrgSection
  , expandDocument, clusterEither, clusterEither'
  , expandImageUrl, createImages
  , isHttpLink, rootHeader
) where

import Data.List
import Models
import Tree

_flipEither :: Either a b -> Either b a
_flipEither (Left x) = Right x
_flipEither (Right x) = Left x

-- Groups adjacent (Right b) elements into one (Right [b]).
clusterEither :: [Either a b] -> [Either a [b]]
clusterEither [] = []
clusterEither (Left a:rest) = Left a : clusterEither rest
clusterEither (Right b:rest) = case clusterEither rest of
  [] -> [Right [b]]
  l@(Left _:_) -> Right [b] : l
  (Right l:rs) -> Right (b:l) : rs

-- Groups adjacent (Left b) elements into one (Left [b]).
clusterEither' :: [Either b a] -> [Either [b] a]
clusterEither' ls = fmap _flipEither <$> clusterEither $ _flipEither <$> ls

isHttpLink :: String -> Bool
isHttpLink st = or $ (`isPrefixOf` st) <$> ["http://", "https://"]

createImages :: OrgLine -> [ImageMetadata MetadataRawString]
createImages (Line lnx rol) = (`ImageMetadata` rol) . MetadataRawString <$> lnx

expandImageUrl :: ImageMetadata MetadataRawString -> ImageMetadata MetadataImage
expandImageUrl (ImageMetadata (MetadataRawString st) desc)
  | isHttpLink st = ImageMetadata (MetadataUrl st) desc
  | otherwise = ImageMetadata (MetadataFilePath st) desc

rootHeader :: OrgHeader
rootHeader = Header (-1) Nothing Nothing "Root Header" Nothing

expandDocument :: [OrgDocLine] -> Maybe OrgSection
expandDocument lns = case structure lns rootHeader of
  ((Right x):_) -> Just x
  _ -> Nothing
