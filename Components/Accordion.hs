{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.Accordion (renderTree) where

import Tree
import Models
import Helpers
import Logic

import Control.Monad.State

import Text.Blaze.Html5 as H hiding (main, style)
import Text.Blaze.Html5.Attributes

_renderHeader :: OrgHeader -> Html
_renderHeader (Header _depth keyword priority hTitle tags) = toMarkup hTitle 

renderTree :: (MonadState RequestState m) => ([b] -> m Html) -> Tree OrgHeader b -> m Html
renderTree fn (Tree hd ls) = do
  currentId <- _makeAccordionId <$> modifyThenGet' accordionIdx (+ 1)
  innerHtml <- mapM g $ clusterEither' ls
  return $ H.div ! class_ "accordion " $ do
    input ! type_ "checkbox" ! Text.Blaze.Html5.Attributes.id (stringValue currentId) ! class_ "d-hide"
    H.label ! class_ "accordion-header" ! style "cursor: pointer" ! for (stringValue currentId) $ do
      i ! class_ "icon icon-arrow-right mr-1" $ ">"
      _renderHeader hd
    H.div ! class_ "accordion-body mx-2" ! style "overflow-y: auto;" $
      mconcat innerHtml where
  g (Left el) = fn el
  g (Right subTree) = renderTree fn subTree

_makeAccordionId :: Int -> String
_makeAccordionId idx = "accordion-" <> show idx

