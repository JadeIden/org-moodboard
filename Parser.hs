{-# LANGUAGE FlexibleContexts #-}

module Parser (parseLine, parseFile, linkLineSplitter) where

import Models
import Text.Parsec
import Control.Monad
import Data.Char (isSpace)
import Control.Monad.Except
import Data.Functor (($>))
import qualified Control.Exception as CE
import System.IO.Error

type Link = String

data TextChunk = LinkChunk Link | TextChunk String

eof' :: Parsec String () a
eof' = eof $> undefined

keywords :: [String]
keywords = ["TODO", "PROJ", "STRT", "WAIT", "HOLD", "|", "DONE", "KILL", "[  ]", "[-]", "[?]", "[X]"]

tagCharacter :: Parsec String () Char
tagCharacter = alphaNum <|> msum (char <$> "_@#%")

keywordsBlock :: Parsec String () String
keywordsBlock = msum (string <$> keywords)

priorityBlock :: Parsec String () Char
priorityBlock = char '[' *> char '#' *> letter <* char ']'

starsBlock :: Parsec String () String
starsBlock = many1 $ char '*'

tagsBlock :: Parsec String () [String]
tagsBlock = char ':' *> sepEndBy1 (many1 tagCharacter) (char ':')

linkC :: Parsec String () TextChunk
linkC = LinkChunk <$> try (string "[[" *> manyTill anyChar (try . lookAhead $ string "]]") <* string "]]")

divideTextChunks :: [TextChunk] -> ([String], [String])
divideTextChunks [] = ([], [])
divideTextChunks (x:xs) = let (links, texts) = divideTextChunks xs in
  case x of
    LinkChunk y -> (y:links, texts)
    TextChunk y -> (links, y:texts)

linkLineSplitter :: Parsec String () ([Link], String)
linkLineSplitter = f . divideTextChunks <$> many (linkC <|> (TextChunk . pure <$> try anyChar)) where
  f (links, texts) = (links, foldl (<>) "" texts)

headerParser :: Parsec String () OrgDocLine
headerParser = do
  stars' <- length <$> starsBlock
  _ <- many space
  keyword' <- optionMaybe $ try keywordsBlock
  _ <- many space
  priority' <- optionMaybe $ try priorityBlock
  _ <- many space
  title' <- trim <$> manyTill anyChar (try $ lookAhead tagsBlock <|> eof')
  _ <- many space
  tags' <- optionMaybe tagsBlock
  return $ Left $ Header stars' keyword' priority' title' tags'

regularLineParser :: Parsec String () OrgDocLine
regularLineParser = Right . uncurry Line <$> linkLineSplitter

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

parseLine :: (MonadError ParseError m) => String -> m OrgDocLine
parseLine ln = liftEither $ parse (headerParser <|> regularLineParser) "parseLine" ln

parseFile :: (MonadError ParseError m, MonadIO m) => String -> m [OrgDocLine]
parseFile fp = do
  rawContents <- liftIO $ readFile fp
  parsedLines <- mapM parseLine $ lines rawContents
  return parsedLines
