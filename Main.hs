{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Layout
import Logic
import Parser
import Config

import Control.Monad.Except
import Data.ByteString.Lazy as Lazy
import Data.ByteString.Lazy.Char8 as LazyC8
import Data.Proxy
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import Text.Blaze.Html.Renderer.Utf8

data HTML
newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type HTMLParameterizedAPI = "html" :> Capture "orgName" String :> Get '[HTML] RawHtml
type StaticAPI = "static" :> Raw

serverStatic :: Server StaticAPI
serverStatic = serveDirectoryWebApp staticDirectory

server :: Server HTMLParameterizedAPI
server fileName = do
  parsedFile <- liftIO $ runExceptT $ expandDocument <$> parseFile fileName -- TODO Add better error message for nonexistent files
  case parsedFile of
    Left parseError -> return . RawHtml $ LazyC8.pack $ show parseError
    Right Nothing -> return . RawHtml . LazyC8.pack $ "Page was empty!"
    Right (Just lns) -> liftIO $ RawHtml . renderHtml <$> generateAccordionPage fileName lns

bigAPI :: Proxy (HTMLParameterizedAPI :<|> StaticAPI)
bigAPI = Proxy

bigServer :: Server (HTMLParameterizedAPI :<|> StaticAPI)
bigServer = server :<|> serverStatic

main :: IO ()
main = (Prelude.putStrLn $ "Running on port " ++ show portNumber ++ "...") *> run portNumber (serve bigAPI bigServer)
