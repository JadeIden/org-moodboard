{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Config where

import Data.Map

data OrgEndpointStrategy = OrgFileWhitelist [FilePath] -- Only allow access to the following files
  | OnlySameDirectory -- Only allow access to org files in this directory
  | OnlySameAndSubdirectories -- Only allow access to org files in this directory or any of its subdirectories

portNumber :: Int
portNumber = 8081

orgEndpointStrategy :: OrgEndpointStrategy --  TODO Not yet implemented
orgEndpointStrategy = OnlySameDirectory

allowedImageTypesMap :: Map String String
allowedImageTypesMap = fromList [
  ("jpg", "image/jpeg"),
  ("jpeg", "image/jpeg"),
  ("png", "image/png")
  ]

staticDirectory :: String
staticDirectory = "static"
